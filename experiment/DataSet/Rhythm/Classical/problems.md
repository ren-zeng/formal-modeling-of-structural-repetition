1. ternary split 
   1. [a1:b1:c1] [a2:b2:c2] [a3:b3:c3] 
      1. c1+a2 =0, c2+a3=0, b1+b2+b3 = b, a1 = a, c3 = c
2. respelling (split rule where one children has zero duration so it was dropped) 
   1. [a1:b1:c1]
      1. (split drop second) a1 = a, b1+c1 = b + c
      2. (split drop first) c1 = c, a1+b1 = a + b
   2. (rewrite the trees to binary) 
      1. c1 + a2 = 0, b1 + b2 = b, a2 + b2 + c2 = 0 


## annotation remark 
termination of NT where upbeat > 0 means need to apply an anticipation
   -{[a,b,c] -term-> x} ~> {[a,b,c] -ant-> [0,b,c+a] -term-> x}



