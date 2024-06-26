library( DiagrammeR)
library(DiagrammeR)

grViz("
  digraph {
    
    # node definitions with substituted label text
    node [fontname = Helvetica, shape = rectangle]
    A [label = 'Study Start']
    B [label = 'Interim Analysis']
    C [label = 'Stop for Success Superiority\n(9%)']
    D [label = 'Stop for Non-inferiority\n(72%)']
    E [label = 'Continue\n(13%)']
    F [label = 'Stop for Futility\n(6%)']
    G [label = 'Final Analysis']
    H [label = 'Success Superiority\n(1%)']
    I [label = 'Success Non-inferiority\n(62%)']
    J [label = 'Futility\n(38%)']
    K [label = 'Success\n(0%)' ]
    L [label = 'Success non-infeirority (but not superiority)\n(8%)' ]
    M [label = 'Futility\n(5%)' ]
    N [label = 'Futility\n(11%)' ]
    O [label = 'Success Superiority\n(9%)']
    P [label = 'Success non-infeirority (but not superiority)\n(80%)' ]
    # edge definitions with the node IDs
    A -> B
    B -> C
    B -> D
    B -> E
    B -> F
    E -> G
    G -> H
    G -> I
    G -> J
    H -> K
    I -> L
    J -> M
    M -> N
    F -> N
    C -> O
    K -> O
    D -> P
    J -> P
    
    
  }
")



# Left to right
library(DiagrammeR)

grViz("
  digraph {
    
    # graph attributes
    graph [rankdir = 'LR']

    # node definitions with substituted label text
    node [fontname = Helvetica, shape = rectangle]
    A [label = 'Study Start']
    B [label = 'Interim Analysis']
    C [label = 'Stop for Success Superiority\n(9%)']
    D [label = 'Stop for Non-inferiority\n(72%)']
    E [label = 'Continue\n(13%)']
    F [label = 'Stop for Futility\n(6%)']
    G [label = 'Final Analysis']
    H [label = 'Success Superiority\n(1%)']
    I [label = 'Success Non-inferiority\n(62%)']
    J [label = 'Futility\n(38%)']
    K [label = 'Success\n(0%)' ]
    L [label = 'Success non-inferiority (but not superiority)\n(8%)' ]
    M [label = 'Futility\n(5%)' ]
    N [label = 'Futility\n(11%)' ]
    O [label = 'Success Superiority\n(9%)']
    P [label = 'Success non-inferiority (but not superiority)\n(80%)' ]

    # edge definitions with the node IDs
    A -> B
    B -> C
    B -> D
    B -> E
    B -> F
    E -> G
    G -> H
    G -> I
    G -> J
    H -> K
    I -> L
    J -> M
    M -> N
    F -> N
    C -> O
    K -> O
    D -> P
    J -> P
  }
")




grViz("
  digraph {
    
    # graph attributes
    graph [rankdir = 'LR']

    # node definitions with substituted label text
    node [fontname = Helvetica, shape = rectangle, style=filled]
    A [label = 'Study Start']
    B [label = 'Interim Analysis',  shape = diamond]
    C [label = 'Stop for Success Superiority\n(9%)', fillcolor ='darkgreen', fontcolor='white']
    D [label = 'Stop for Non-inferiority (not supriority(\n(72%)', fillcolor='#2AD778']
    E [label = 'Continue\n(13%)', fillcolor='yellow']
    F [label = 'Stop for Futility\n(6%)', fillcolor = 'red', fontcolor='white']
    G [label = 'Final Analysis', shape = diamond]
    H [label = 'Success Superiority\n(1%)', fillcolor ='darkgreen', fontcolor='white' ]
    I [label = 'Success Non-inferiority\n(62%)' fillcolor='#2AD778']
    J [label = 'Futility\n(38%)', fillcolor = 'red' , fontcolor='white']
    K [label = 'Success\n(0%)', fillcolor ='darkgreen', fontcolor='white' ]
    L [label = 'Success non-inferiority (but not superiority)\n(8%)',  fillcolor='#2AD778' ]
    M [label = 'Futility\n(5%)' , fillcolor = 'red' , fontcolor='white']
    N [label = 'Futility\n(11%)' , fillcolor = 'red', fontcolor='white']
    O [label = 'Success Superiority\n(9%)', fillcolor ='darkgreen', fontcolor='white']
    P [label = 'Success non-inferiority (but not superiority)\n(80%)',  fillcolor='#2AD778' ]

    # edge definitions with the node IDs
    A -> B
    B -> C [weight=1]
    B -> D [weight=1]
    B -> E [weight=1]
    B -> F [weight=1]
    E -> G
    G -> H
    G -> I
    G -> J
    H -> K
    I -> L
    J -> M
    M -> N
    F -> N
    C -> O
    K -> O
    D -> P
    L -> P
    # subgraph for vertical alignment
    {
      rank=same; C; D; E; F;
    }
    
    {
      rank=same; H; I; J;
    }
    
    {
      rank=same; K; L; M;
    }
    {
      rank=same; N; O; P;
    }
  }
")
