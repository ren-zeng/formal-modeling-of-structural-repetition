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

  // Toggle state for persistent tooltips
  let toggledNode = null;
  let isToggledMode = false;

  // Create tree tooltip function that renders SVG
  function createTreeTooltipSVG(ruleTree) {
    if (!ruleTree || ruleTree.length < 2) return null;
    
    const rootNode = ruleTree[0];
    const rootChildren = ruleTree[1];
    
    // Parse the tree structure first
    function parseTree(item) {
      if (Array.isArray(item)) {
        // This is an array containing [node, children]
        if (item.length >= 1) {
          const node = item[0];
          const children = item.length > 1 ? item[1] : [];
          
          if (node && typeof node === 'object' && !Array.isArray(node)) {
            // Handle new format with tagged nodes (Hole/Constant)
            let nodeText, nodeTag;
            if (node.tag === "Hole") {
              nodeText = "○"; // Circle symbol for holes
              nodeTag = "Hole";
            } else if (node.tag === "Constant" && node.contents) {
              // For Constant nodes, use the contents
              nodeText = node.contents.contents || node.contents.tag || "Constant";
              nodeTag = node.contents.tag || "Constant";
            } else {
              // Fallback for other formats
              nodeText = node.contents || node.tag || "Node";
              nodeTag = node.tag;
            }
            
            const result = {
              name: nodeText,
              tag: nodeTag,
              nodeType: node.tag, // Store whether it's Hole or Constant
              children: []
            };
            
            if (Array.isArray(children)) {
              for (const child of children) {
                const parsedChild = parseTree(child);
                if (parsedChild) result.children.push(parsedChild);
              }
            }
            return result;
          }
        }
      } else if (item && typeof item === 'object') {
        // This is a standalone node object
        let nodeText, nodeTag, nodeType;
        if (item.tag === "Hole") {
          nodeText = "○"; // Circle symbol for holes
          nodeTag = "Hole";
          nodeType = "Hole";
        } else if (item.tag === "Constant" && item.contents) {
          nodeText = item.contents.contents || item.contents.tag || "Constant";
          nodeTag = item.contents.tag || "Constant";
          nodeType = "Constant";
        } else {
          nodeText = item.contents || item.tag || "Node";
          nodeTag = item.tag;
          nodeType = item.tag;
        }
        
        return {
          name: nodeText,
          tag: nodeTag,
          nodeType: nodeType,
          children: []
        };
      }
      return null;
    }
    
    // Create the tree data structure
    let treeData;
    if (rootNode.tag === "Hole") {
      treeData = {
        name: "○",
        tag: "Hole",
        nodeType: "Hole",
        children: []
      };
    } else if (rootNode.tag === "Constant" && rootNode.contents) {
      treeData = {
        name: rootNode.contents.contents || rootNode.contents.tag || "Constant",
        tag: rootNode.contents.tag || "Constant",
        nodeType: "Constant",
        children: []
      };
    } else {
      treeData = {
        name: rootNode.contents || rootNode.tag || "Root",
        tag: rootNode.tag,
        nodeType: rootNode.tag,
        children: []
      };
    }
    
    if (Array.isArray(rootChildren)) {
      for (const child of rootChildren) {
        const parsedChild = parseTree(child);
        if (parsedChild) treeData.children.push(parsedChild);
      }
    }
    
    // Calculate tree dimensions based on content
    function calculateTreeDimensions(root) {
      const hierarchy = d3.hierarchy(root);
      const maxDepth = hierarchy.height;
      const leaves = hierarchy.leaves().length;
      const totalNodes = hierarchy.descendants().length;
      
      // Calculate dynamic dimensions with more generous spacing
      const minWidth = 300;
      const minHeight = 200;
      const nodeWidth = 100; // Increased width per node for better spacing
      const nodeHeight = 60; // Increased height per level for better spacing
      
      // Allow unlimited expansion based on content
      const calculatedWidth = Math.max(minWidth, leaves * nodeWidth);
      const calculatedHeight = Math.max(minHeight, (maxDepth + 1) * nodeHeight + 120);
      
      return {
        width: calculatedWidth, // No maximum limit
        height: calculatedHeight, // No maximum limit
        maxDepth,
        totalNodes
      };
    }
    
    const dimensions = calculateTreeDimensions(treeData);
    const treeWidth = dimensions.width;
    const treeHeight = dimensions.height;
    const fontSize = 11;
    const nodeSpacing = 30; // Increased minimum horizontal spacing between nodes
    const levelSpacing = 60; // Increased vertical spacing between levels
    
    const treeSvg = d3.create("svg")
      .attr("width", treeWidth)
      .attr("height", treeHeight)
      .style("background", "rgba(0, 0, 0, 0.9)")
      .style("border-radius", "5px");
    
    // Create tree layout with better spacing
    const treeLayout = d3.tree()
      .size([treeWidth - 40, treeHeight - 60])
      .separation((a, b) => {
        // Increase separation to prevent overlap
        const aWidth = a.data.name.length * 7 + 16; // Approximate node width
        const bWidth = b.data.name.length * 7 + 16;
        const minSeparation = (aWidth + bWidth) / 2 + 10; // Minimum separation
        return Math.max(1, minSeparation / 80); // Normalize to tree layout scale
      });
    
    const hierarchy = d3.hierarchy(treeData);
    const treeNodes = treeLayout(hierarchy);
    
    const g = treeSvg.append("g")
      .attr("transform", "translate(20, 30)");
    
    // Add straight line links
    g.selectAll(".tree-link")
      .data(treeNodes.links())
      .enter().append("line")
      .attr("class", "tree-link")
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y)
      .style("stroke", "#999")
      .style("stroke-width", 1.5);
    
    // Add nodes as text labels with better positioning
    const node = g.selectAll(".tree-node")
      .data(treeNodes.descendants())
      .enter().append("g")
      .attr("class", "tree-node")
      .attr("transform", d => `translate(${d.x},${d.y})`);
    
    // Add background shapes - calculate size based on text length
    node.each(function(d) {
      const nodeElement = d3.select(this);
      const textLength = d.data.name.length;
      const nodeWidth = Math.max(textLength * 7 + 16, 30); // Minimum width of 30
      const nodeHeight = fontSize + 8;
      
      if (d.data.nodeType === "Hole") {
        // Hole nodes have no background shape - just text
        // No circle or rectangle added
      } else {
        // Render Constant and other nodes as rectangles
        nodeElement.append("rect")
          .attr("width", nodeWidth)
          .attr("height", nodeHeight)
          .attr("x", -nodeWidth / 2)
          .attr("y", -nodeHeight / 2)
          .attr("rx", 4)
          .style("fill", d => {
            if (!d.data.tag) return "rgba(204, 204, 204, 0.8)";
            switch(d.data.tag) {
              case "Prep": return "rgba(255, 107, 107, 0.8)";
              case "Prol": return "rgba(78, 205, 196, 0.8)"; 
              case "Term": return "rgba(69, 183, 209, 0.8)";
              case "Constant": return "rgba(100, 200, 100, 0.8)"; // Green for constants
              default: return "rgba(254, 202, 87, 0.8)";
            }
          })
          .style("stroke", "#fff")
          .style("stroke-width", 1.5);
      }
    });
    
    // Add rule labels as the main node text
    node.append("text")
      .attr("text-anchor", "middle")
      .attr("dy", "0.35em")
      .style("font-family", "monospace")
      .style("font-size", `${fontSize}px`)
      .style("fill", "white")
      .style("font-weight", "bold")
      .style("text-shadow", "1px 1px 1px rgba(0,0,0,0.8)")
      .text(d => d.data.name);
    
    return treeSvg.node();
  }

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

  // Create tooltip div for SVG content
  const tooltip = d3.select("body").append("div")
    .attr("class", "tree-tooltip")
    .style("position", "absolute")
    .style("visibility", "hidden")
    .style("padding", "0")
    .style("border-radius", "5px")
    .style("z-index", "1000")
    .style("pointer-events", "none");

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
      .style("cursor", "pointer")  // Indicate clickability
      .on("click", function(event, d) {
        // Toggle behavior on click
        if (toggledNode === d) {
          // If clicking the same node, untoggle it
          toggledNode = null;
          isToggledMode = false;
          tooltip.style("visibility", "hidden");
          // Reset visual state
          d3.select(this)
            .attr("font-weight", null)
            .style("text-decoration", "none");
          resetAllEdges();
        } else {
          // Toggle to new node
          // First reset previous toggled node visual state
          if (toggledNode) {
            d3.select(toggledNode.text)
              .attr("font-weight", null)
              .style("text-decoration", "none");
          }
          
          toggledNode = d;
          isToggledMode = true;
          
          // Set visual state for toggled node
          d3.select(this)
            .attr("font-weight", "bold")
            .style("text-decoration", "underline");
          
          // Show persistent tooltip
          if (d.data.ruleTree) {
            const treeSvg = createTreeTooltipSVG(d.data.ruleTree);
            if (treeSvg) {
              tooltip
                .style("visibility", "visible")
                .html("")
                .node().appendChild(treeSvg);
              
              // Calculate position next to the pattern node, outside the circle
              const nodeAngle = d.x;
              const nodeRadius = d.y;
              
              // Convert polar coordinates to cartesian (relative to SVG center)
              const nodeX = nodeRadius * Math.cos(nodeAngle - Math.PI/2);
              const nodeY = nodeRadius * Math.sin(nodeAngle - Math.PI/2);
              
              // Calculate SVG center position on page
              const svgElement = svg.node();
              const svgRect = svgElement.getBoundingClientRect();
              const svgCenterX = svgRect.left + svgRect.width / 2;
              const svgCenterY = svgRect.top + svgRect.height / 2;
              
              // Convert to page coordinates
              const pageX = svgCenterX + nodeX;
              const pageY = svgCenterY + nodeY;
              
              // Position tooltip outside the circle from the node
              const tooltipWidth = 400;
              const tooltipHeight = 250;
              const offsetDistance = 50; // Distance from the node
              
              // Calculate offset direction (away from center)
              const offsetAngle = nodeAngle - Math.PI/2;
              const offsetX = Math.cos(offsetAngle) * offsetDistance;
              const offsetY = Math.sin(offsetAngle) * offsetDistance;
              
              let finalX = pageX + offsetX;
              let finalY = pageY + offsetY;
              
              // Adjust position to keep tooltip on screen
              const screenWidth = window.innerWidth;
              const screenHeight = window.innerHeight;
              
              // Prevent tooltip from going off the right edge
              if (finalX + tooltipWidth > screenWidth) {
                finalX = pageX - offsetX - tooltipWidth;
              }
              
              // Prevent tooltip from going off the bottom edge
              if (finalY + tooltipHeight > screenHeight) {
                finalY = Math.max(10, finalY - tooltipHeight);
              }
              
              // Prevent tooltip from going off the left edge
              if (finalX < 0) {
                finalX = 10;
              }
              
              // Prevent tooltip from going off the top edge
              if (finalY < 0) {
                finalY = 10;
              }
              
              tooltip
                .style("left", finalX + "px")
                .style("top", finalY + "px");
            }
          }
          
          // Highlight edges for toggled node
          highlightNodeEdges(d);
        }
        event.stopPropagation(); // Prevent event bubbling
      })
      .on("mouseover", overed)
      .on("mouseout", outed)
      .on("mousemove", function(event, d) {
        // Only show hover tooltip if not in toggled mode
        if (!isToggledMode) {
          if (d.data.ruleTree) {
            const treeSvg = createTreeTooltipSVG(d.data.ruleTree);
            if (treeSvg) {
              tooltip
                .style("visibility", "visible")
                .html("")
                .node().appendChild(treeSvg);
              
              tooltip
                .style("left", (event.pageX + 10) + "px")
                .style("top", (event.pageY - 10) + "px");
            }
          }
        }
      })
      .call(text => text.append("title").text(d => `${d.data.patternID}
Level: ${d.data.level}
Dependencies: ${d.data.dependenciesDirect ? d.data.dependenciesDirect.length : 0}
Dependents: ${d.data.dependentsDirect ? d.data.dependentsDirect.length : 0}
Global Frequency: ${d.data.globalFreq || 0}
${d.outgoing.length} outgoing connections
${d.incoming.length} incoming connections
Click to toggle persistent tree view`));

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
    // Don't highlight on hover if in toggled mode
    if (isToggledMode) {
      return;
    }
    
    highlightNodeEdges(d);
    d3.select(this).attr("font-weight", "bold");
  }

  function outed(event, d) {
    // Don't reset if in toggled mode
    if (isToggledMode) {
      return;
    }
    
    // Hide tooltip only if not in toggled mode
    tooltip.style("visibility", "hidden");
    
    link.style("mix-blend-mode", "normal");
    d3.select(this).attr("font-weight", null);
    
    // Reset all edges to normal appearance
    resetAllEdges();
  }

  const highlightNodeEdges = (d) => {
    link.style("mix-blend-mode", null);
    
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

  const resetAllEdges = () => {
    link.style("mix-blend-mode", "normal");
    
    // Reset all edges to normal appearance
    link
      .attr("stroke-opacity", EDGE_OPACITY)
      .attr("stroke-width", EDGE_WIDTH);
    
    // Reset all node colors to original
    actualNodes.forEach(node => {
      d3.select(node.text)
        .attr("fill", node.originalColor)
        .attr("font-weight", null);
    });
  }

  // Add click handler to SVG background to clear toggle when clicking empty space
  svg.on("click", function(event) {
    if (event.target === this && isToggledMode) {
      // Clear toggle state
      if (toggledNode) {
        d3.select(toggledNode.text)
          .attr("font-weight", null)
          .style("text-decoration", "none");
      }
      toggledNode = null;
      isToggledMode = false;
      tooltip.style("visibility", "hidden");
      resetAllEdges();
    }
  });

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
