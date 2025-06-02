function _1(colorout,colorin,md){return(
md`<div style="color: grey; font: 13px/25.5px var(--sans-serif); text-transform: uppercase;"><h1 style="display: none;">Hierarchical edge bundling</h1><a href="https://d3js.org/">D3</a> › <a href="/@d3/gallery">Gallery</a></div>

# Hierarchical edge bundling

This chart shows relationships among classes in a software hierarchy. Hover a class to reveal its imports (<b style="color: ${colorout};">outgoing</b> edges) and classes that import it (<b style="color: ${colorin};">incoming</b> edges).`
)}

function _chart(d3,bilink,data,id,colornone,colorin,colorout)
{
  // Configuration constants
  const EDGE_WIDTH = 2;
  const EDGE_WIDTH_HOVER = 3;
  const EDGE_OPACITY = 0.3;
  const EDGE_OPACITY_HOVER = 1;
  
  // Gradient color constants
  const GRADIENT_SOURCE_COLOR = "#ff0000";  // Red for source (dependencies)
  const GRADIENT_MIDDLE_COLOR = "#f5f5f5";  // Light grey for middle
  const GRADIENT_TARGET_COLOR = "#0000ff";  // Blue for target (dependents)
  const GRADIENT_MIDDLE_START = "50%";      // Where middle color starts
  const GRADIENT_MIDDLE_END = "50%";        // Where middle color ends
  
  // Level color constants - easily customizable color palette
  const LEVEL_COLORS = [
    "#e41a1c", // Level 0: bright red
    "#377eb8", // Level 1: bright blue
    "#4daf4a", // Level 2: bright green
    "#984ea3", // Level 3: bright purple
    "#ff7f00", // Level 4: bright orange
    "#999999", // Level 5: bright yellow
    "#a65628", // Level 6: brown
    "#f781bf", // Level 7: pink
    "#999999", // Level 8: grey
    "#1f78b4", // Level 9: dark blue
    "#33a02c", // Level 10: dark green
    "#fb9a99", // Level 11: light red
    "#fdbf6f", // Level 12: light orange
    "#cab2d6", // Level 13: light purple
    "#6a3d9a", // Level 14: dark purple
    "#b15928", // Level 15: dark brown
    "#a6cee3", // Level 16: light blue
    "#b2df8a", // Level 17: light green
  ];
  
  const width = 954;
  const radius = width / 2;

  const tree = d3.cluster()
    .size([2 * Math.PI, radius - 100]);
  const root = tree(bilink(d3.hierarchy(data)
      .sort((a, b) => d3.ascending(a.height, b.height) || d3.ascending(parseInt(a.data.patternID), parseInt(b.data.patternID)))));

  // Filter out virtual nodes for display
  const actualNodes = root.leaves().filter(d => !d.data.isVirtual);
  
  // Create color scale based on topological levels
  const levels = actualNodes.map(d => d.data.level);
  const minLevel = Math.min(...levels);
  const maxLevel = Math.max(...levels);
  
  // Create a more distinguishable color scale with better contrast
  // Using the customizable color palette defined above
  const levelColorScale = (level) => {
    // If we have more levels than colors, cycle through them
    return LEVEL_COLORS[level % LEVEL_COLORS.length];
  };

  const svg = d3.create("svg")
      .attr("width", width)
      .attr("height", width)
      .attr("viewBox", [-width / 2, -width / 2, width, width])
      .attr("style", "max-width: 100%; height: auto; font: 10px sans-serif;");

  // Create gradient definitions for each edge
  const defs = svg.append("defs");
  
  const allEdges = actualNodes.flatMap(leaf => leaf.outgoing);

  // Create a unique gradient for each edge
  allEdges.forEach((edge, i) => {
    const [source, target] = edge;
    
    const gradient = defs.append("linearGradient")
      .attr("id", `gradient-${i}`)
      .attr("gradientUnits", "userSpaceOnUse")
      .attr("x1", source.y * Math.cos(source.x - Math.PI/2))
      .attr("y1", source.y * Math.sin(source.x - Math.PI/2))
      .attr("x2", target.y * Math.cos(target.x - Math.PI/2))
      .attr("y2", target.y * Math.sin(target.x - Math.PI/2));
    
    gradient.append("stop")
      .attr("offset", "0%")
      .attr("stop-color", GRADIENT_SOURCE_COLOR); // Bright red for source
    
    gradient.append("stop")
      .attr("offset", GRADIENT_MIDDLE_START)
      .attr("stop-color", GRADIENT_MIDDLE_COLOR); // Very light grey starts earlier
    
    gradient.append("stop")
      .attr("offset", GRADIENT_MIDDLE_END)
      .attr("stop-color", GRADIENT_MIDDLE_COLOR); // Very light grey ends later
    
    gradient.append("stop")
      .attr("offset", "100%")
      .attr("stop-color", GRADIENT_TARGET_COLOR); // Bright blue for target
  });

  const node = svg.append("g")
    .selectAll()
    .data(actualNodes)
    .join("g")
      .attr("transform", d => `rotate(${d.x * 180 / Math.PI - 90}) translate(${d.y},0)`)
    .append("text")
      .attr("dy", "0.31em")
      .attr("x", d => d.x < Math.PI ? 6 : -6)
      .attr("text-anchor", d => d.x < Math.PI ? "start" : "end")
      .attr("transform", d => d.x >= Math.PI ? "rotate(180)" : null)
      .attr("fill", d => levelColorScale(d.data.level))  // Color by topological level
      .text(d => d.data.patternID)
      .each(function(d) { 
        d.text = this; 
        d.originalColor = levelColorScale(d.data.level); // Store original color
      })
      .on("mouseover", overed)
      .on("mouseout", outed)
      .call(text => text.append("title").text(d => `${d.data.patternID}
Level: ${d.data.level}
Dependencies: ${d.data.dependenciesDirect ? d.data.dependenciesDirect.length : 0}
Dependents: ${d.data.dependentsDirect ? d.data.dependentsDirect.length : 0}
Global Frequency: ${d.data.globalFreq || 0}
${d.outgoing.length} outgoing connections
${d.incoming.length} incoming connections`));

  const line = d3.lineRadial()
    .curve(d3.curveBundle.beta(0.85))
    .radius(d => d.y)
    .angle(d => d.x);

  const link = svg.append("g")
      .attr("fill", "none")
    .selectAll()
    .data(allEdges)
    .join("path")
      .style("mix-blend-mode", "normal")
      .attr("stroke", (d, i) => `url(#gradient-${i})`)
      .attr("stroke-width", EDGE_WIDTH)
      .attr("stroke-opacity", EDGE_OPACITY)
      .attr("d", ([i, o]) => line(i.path(o)))
      .each(function(d, i) { 
        d.path = this; 
        d.gradientId = `gradient-${i}`;
      });

  function overed(event, d) {
    link.style("mix-blend-mode", null);
    d3.select(this).attr("font-weight", "bold");
    
    // Highlight incoming edges with enhanced visibility
    d3.selectAll(d.incoming.map(d => d.path))
      .attr("stroke-opacity", EDGE_OPACITY_HOVER)
      .attr("stroke-width", EDGE_WIDTH_HOVER)
      .raise();
    d3.selectAll(d.incoming.map(([d]) => d.text))
      .attr("fill", colorout)
      .attr("font-weight", "bold");
    
    // Highlight outgoing edges with enhanced visibility
    d3.selectAll(d.outgoing.map(d => d.path))
      .attr("stroke-opacity", EDGE_OPACITY_HOVER)
      .attr("stroke-width", EDGE_WIDTH_HOVER)
      .raise();
    d3.selectAll(d.outgoing.map(([, d]) => d.text))
      .attr("fill", colorin)
      .attr("font-weight", "bold");
  }

  function outed(event, d) {
    link.style("mix-blend-mode", "normal");
    d3.select(this).attr("font-weight", null);
    
    // Reset all edges to normal appearance
    d3.selectAll(d.incoming.map(d => d.path))
      .attr("stroke-opacity", EDGE_OPACITY)
      .attr("stroke-width", EDGE_WIDTH);
    d3.selectAll(d.incoming.map(([d]) => d.text))
      .attr("fill", d => d.originalColor)  // Restore original level-based color
      .attr("font-weight", null);
    
    d3.selectAll(d.outgoing.map(d => d.path))
      .attr("stroke-opacity", EDGE_OPACITY)
      .attr("stroke-width", EDGE_WIDTH);
    d3.selectAll(d.outgoing.map(([, d]) => d.text))
      .attr("fill", d => d.originalColor)  // Restore original level-based color
      .attr("font-weight", null);
  }

  return svg.node();
}


async function _data(hierarchy,FileAttachment){return(
hierarchy(await FileAttachment("mydata").json())
)}

function _hierarchy(){return(
function hierarchy(data) {
  // Create a map of all nodes
  const nodeMap = new Map();
  data.forEach(d => {
    nodeMap.set(d.patternID, {
      ...d,
      children: [],
      level: -1 // Will be calculated
    });
  });

  // Calculate dependency levels using topological sort
  const calculateLevels = () => {
    const visited = new Set();
    const visiting = new Set();
    
    const dfs = (nodeId) => {
      if (visiting.has(nodeId)) {
        // Circular dependency detected, assign level 0
        return 0;
      }
      if (visited.has(nodeId)) {
        return nodeMap.get(nodeId).level;
      }
      
      visiting.add(nodeId);
      const node = nodeMap.get(nodeId);
      
      if (!node.dependenciesDirect || node.dependenciesDirect.length === 0) {
        // No dependencies, this is a root node
        node.level = 0;
      } else {
        // Level is max dependency level + 1
        let maxDepLevel = -1;
        for (const depId of node.dependenciesDirect) {
          if (nodeMap.has(depId)) {
            const depLevel = dfs(depId);
            maxDepLevel = Math.max(maxDepLevel, depLevel);
          }
        }
        node.level = maxDepLevel + 1;
      }
      
      visiting.delete(nodeId);
      visited.add(nodeId);
      return node.level;
    };
    
    // Calculate levels for all nodes
    nodeMap.forEach((node, nodeId) => {
      if (!visited.has(nodeId)) {
        dfs(nodeId);
      }
    });
  };

  calculateLevels();

  // Group nodes by level
  const levelGroups = new Map();
  nodeMap.forEach(node => {
    if (!levelGroups.has(node.level)) {
      levelGroups.set(node.level, []);
    }
    levelGroups.get(node.level).push(node);
  });

  // Create hierarchical structure
  const maxLevel = Math.max(...Array.from(levelGroups.keys()));
  
  // Create virtual root and level containers
  const createLevelNode = (level, children) => ({
    patternID: `Level_${level}`,
    isVirtual: true,
    level: level,
    children: children.sort((a, b) => a.patternID.localeCompare(b.patternID))
  });

  const root = {
    patternID: 'Root',
    isVirtual: true,
    level: -1,
    children: []
  };

  // Add level containers to root
  for (let level = 0; level <= maxLevel; level++) {
    if (levelGroups.has(level)) {
      const levelNodes = levelGroups.get(level);
      const levelContainer = createLevelNode(level, levelNodes);
      root.children.push(levelContainer);
    }
  }

  return root;
}
)}

function _bilink(id){return(
function bilink(root) {
  const map = new Map(root.leaves().map(d => [d.data.patternID, d]));
  for (const d of root.leaves()) {
    d.incoming = [];
    d.outgoing = [];
    
    // Only process actual pattern nodes (not virtual level containers)
    if (!d.data.isVirtual && d.data.dependenciesDirect) {
      d.outgoing = d.data.dependenciesDirect
        .map(depId => [d, map.get(depId)])
        .filter(([source, target]) => target); // Only include existing targets
    }
  }
  
  // Build incoming connections
  for (const d of root.leaves()) {
    for (const o of d.outgoing) {
      o[1].incoming.push(o);
    }
  }
  
  return root;
}
)}

function _id(){return(
function id(node) {
  return `${node.parent ? id(node.parent) + "." : ""}${node.data.patternID}`;
}
)}

function _colorin(){return(
"#00f"
)}

function _colorout(){return(
"#f00"
)}

function _colornone(){return(
"#ccc"
)}

export default function define(runtime, observer) {
  const main = runtime.module();
  function toString() { return this.url; }
  const fileAttachments = new Map([
    ["mydata",{url: new URL("./files/patternInfo.json", import.meta.url), mimeType: "application/json", toString}],

    // ["flare.json", {url: new URL("./files/9b6806e3dd9c4c2c26760ba784437138c78b43a9a8e58a0bbafe5833026e3265637c9c7810224d66b79ba907b4d0be731c1a81ad043e10376aec3c18a49f3d84.json", import.meta.url), mimeType: "application/json", toString}]
  ]);
  main.builtin("FileAttachment", runtime.fileAttachments(patternID => fileAttachments.get(patternID)));
  main.variable(observer()).define(["colorout","colorin","md"], _1);
  main.variable(observer("chart")).define("chart", ["d3","bilink","data","id","colornone","colorin","colorout"], _chart);
  main.variable(observer("data")).define("data", ["hierarchy","FileAttachment"], _data);
  main.variable(observer("hierarchy")).define("hierarchy", _hierarchy);
  main.variable(observer("bilink")).define("bilink", ["id"], _bilink);
  main.variable(observer("id")).define("id", _id);
  main.variable(observer("colorin")).define("colorin", _colorin);
  main.variable(observer("colorout")).define("colorout", _colorout);
  main.variable(observer("colornone")).define("colornone", _colornone);
  return main;
}
