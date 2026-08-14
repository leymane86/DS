#Eli Leyman
#CS 580
#Astart-minmax
open_list = []
closed = []
def remove_numbers(s: str) -> str:
    return ''.join(ch for ch in s if not ch.isdigit())
class Node:
    def __init__(self, d):
        self.data = d
        self.val = 0
        self.left = None
        self.vl = 0   # edge cost to left
        self.right = None
        self.vr = 0   # edge cost to right

class State:
    def __init__(self, node, parent, g, h, depth):
        self.node = node
        self.parent = parent
        self.g = g              # cost so far
        self.h = h              # heuristic
        self.f = g + h          # evaluation
        self.depth = depth      # depth in the search tree

def astar_minmax(start_node, goal):
    open_list.clear()
    closed.clear()

    h = 0
    start_state = State(start_node, None, 0, start_node.val, depth=0)
    open_list.append(start_state)
    current=open_list[0]
    while open_list:
        # choose depending on depth (turn stored in state)
# pick a candidate first
        # now correct it based on depth
        
        if ((current.depth+1)%2 == 0):   # even depth → MAX turn
          
            current = min(open_list, key=lambda s: s.f)
            #print("min "+current.node.data, current.f)
        else:                        # odd depth → MIN turn
            current = max(open_list, key=lambda s: s.f)
            #if(current.node.data!=start_node.data):
                #print("max "+current.node.data, current.f)
            #else:
                #print("root "+current.node.data, current.f)
        

        if current.node.data == goal:
            # reconstruct path
            path = []
            while current:
                p = remove_numbers(current.node.data)
                path.append(p)
                #print("Parent "+current.node.data)
                current = current.parent
            #return path[::-1]
            return(" -> ".join(path[::-1]))
        open_list.remove(current)
        closed.append(current.node)

        # expand neighbors
        for child, edge_cost in [(current.node.left, current.node.vl),
                                 (current.node.right, current.node.vr)]:
            if not child:
                continue
            g = current.g + edge_cost
            h = child.val    # placeholder heuristic
            #print("adding child "+child.data+" "+str(g)+" "+str(h)+" "+current.node.data)
            new_state = State(child, current, g, h, current.depth + 1)

            if child in closed:
                continue

            existing = next((s for s in open_list if s.node == child), None)
            #not sure if this is needed becaus of min/max. just updating parent
            #if existing:   
            #    existing.g = g
            #    existing.h = h
            #    existing.f=g+h
            #    existing.depth=current.depth+1
            #    existing.parent = current
            #if not existing:
            open_list.append(new_state)

    return None

hueristics = {"A": 100, "B": 80, "C": 80, "D":60, "E":60, "F":40, "G":60, "H":40}

# Initialize and allocate memory for tree nodes
A_Node = Node("A")
A_Node.vl= 10
A_Node.vr = 5
A_Node.val = hueristics["A"]
B_Node = Node("B")
B_Node.vl=15
B_Node.val=hueristics["B"]
C_Node = Node("C")
C_Node.vl=30
C_Node.vr=15
C_Node.val=hueristics["C"]
D_Node = Node("D")
D_Node.vl = 25
D_Node.val=hueristics["D"]
E_Node = Node("E")
E_Node.vl=10
E_Node.val=hueristics["E"]
F_Node = Node("F")
F_Node.vl=20
F_Node.val=hueristics["F"]
G_Node = Node("G")
G_Node.vl = 20
G_Node.val=hueristics["G"]
H_Node = Node("H")
H_Node.vl = 25
H_Node.val=hueristics["H"]
I_Node = Node("I")


# Connect binary tree nodes
A_Node.left = B_Node
A_Node.right = C_Node
B_Node.left = G_Node
G_Node.left = H_Node
H_Node.left = I_Node
C_Node.left = D_Node
C_Node.right = E_Node
D_Node.left = F_Node
E_Node.left = F_Node
F_Node.left = I_Node


# Initialize nodes
A2 = Node("A2")
B2 = Node("B2")
C2 = Node("C2")
D2 = Node("D2")
E2 = Node("E2")
F2 = Node("F2")
G2 = Node("G2")
H2 = Node("H2")
hueristics = {"A2": 50, "B2": 40, "C2": 30, "D2":20, "E2":25, "F2":15, "G2":10}

# Assign values and edge costs
A2.val, A2.vl, A2.vr = hueristics["A2"], 5, 10
B2.val, B2.vl, B2.vr = hueristics["B2"], 2, 3
C2.val, C2.vl, C2.vr = hueristics["C2"], 6, 1
D2.val = hueristics["D2"]
E2.val = hueristics["E2"]
E2.vr = 8
F2.val = hueristics["F2"]
F2.vl = 2
G2.val = hueristics["G2"]


# Connect nodes
A2.left, A2.right = B2, C2
B2.left, B2.right = D2, E2
C2.left, C2.right = F2, G2
E2.right=H2
F2.left=H2



# Initialize nodes
A3 = Node("A3")
B3 = Node("B3")
C3 = Node("C3")
D3 = Node("D3")
E3 = Node("E3")

hueristics = {"A3": 90, "B3": 70, "C3": 50, "D3":30, "E3":10}

# Assign values and edge costs
A3.val, A3.vl =  hueristics["A3"], 4
B3.val, B3.vl =  hueristics["B3"], 8
C3.val, C3.vl =  hueristics["C3"], 6
D3.val, D3.vl =  hueristics["D3"], 2
E3.val =  hueristics["E3"]

# Connect nodes (skewed like a linked list)
A3.left = B3
B3.left = C3
C3.left = D3
D3.left = E3

print("This is a demonstration of the A* with minmax algorithm")
print("Example 1 Path:", astar_minmax(A_Node, "I"))


print("Example 2 Path:", astar_minmax(A2, "H2"))


print("Example 3 Path:", astar_minmax(A3, "E3"))

