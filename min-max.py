#Eli Leyman
#CS 580
#minmax
class Node:
    def __init__(self, d, val=0):
        self.data = d
        self.val = val       # leaf value (utility)
        self.left = None
        self.right = None

def max_node(left, right):
    if left is None and right is None:
        return None
    if left is None:
        return right
    if right is None:
        return left
    return left if left.val > right.val else right


def min_node(left, right):
    if left is None and right is None:
        return None
    if left is None:
        return right
    if right is None:
        return left
    return left if left.val < right.val else right

def minmax(turn, node):
    if node is None:
        return None, []

    # Base case: leaf node → return itself and path [node]
    if node.left is None and node.right is None:
        return node, [node.data]

    if turn == "MAX":
        left_choice, left_path = minmax("MIN", node.left)
        right_choice, right_path = minmax("MIN", node.right)
        chosen = max_node(left_choice, right_choice)

        if chosen == left_choice:
            path = [node.data] + left_path
        else:
            path = [node.data] + right_path

        #print(f"MAX at {node.data} chooses {chosen.data}")
        return chosen, path

    elif turn == "MIN":
        left_choice, left_path = minmax("MAX", node.left)
        right_choice, right_path = minmax("MAX", node.right)
        chosen = min_node(left_choice, right_choice)

        if chosen == left_choice:
            path = [node.data] + left_path
        else:
            path = [node.data] + right_path

        #print(f"MIN at {node.data} chooses {chosen.data}")
        return chosen, path
    
    
        
# Initialize and allocate memory for tree nodes
A_Node = Node("A")
A_Node.vl= 10
A_Node.vr = 5
B_Node = Node("B")
B_Node.vl=15
C_Node = Node("C")
C_Node.vl=30
C_Node.vr=15
D_Node = Node("D")
D_Node.vr = 25
E_Node = Node("E")
E_Node.vl=10
F_Node = Node("F")
F_Node.vl=20
G_Node = Node("G")
G_Node.vr = 20
H_Node = Node("H")
H_Node.right = 25
I_Node = Node("I")


# Connect binary tree nodes
A_Node.left = B_Node
A_Node.right = C_Node
B_Node.left = G_Node
G_Node.right = H_Node
H_Node.right = I_Node
C_Node.left = D_Node
C_Node.right = E_Node
D_Node.right = F_Node
E_Node.left = F_Node
F_Node.left = I_Node

#minmax
I1_Node = Node("I")
I1_Node.val = 65
I2_Node = Node("I")
I2_Node.val = 85
I3_Node = Node("I")
I3_Node.val = 45
F1_Node = Node("F")
F2_Node = Node("F")
D_Node.left = F1_Node
E_Node.left = F2_Node
H_Node.left = I1_Node
F1_Node.left = I2_Node
F2_Node.left = I3_Node



# Initialize nodes
A2 = Node("A")
B2 = Node("B")
C2 = Node("C")
D2 = Node("D")
E2 = Node("E")
F2 = Node("F")
G2 = Node("G")
H2 = Node("H")
H3 = Node("H")

# Assign values and edge costs
A2.vl, A2.vr =  5, 10
B2.vl, B2.vr = 2, 3
C2.vl, C2.vr = 6, 1
E2.vr = 8
F2.vl = 2
H2.val= 16
H3.val=18
D2.val=1000
G2.val=1000

# Connect nodes
A2.left, A2.right = B2, C2
B2.left, B2.right = D2, E2
C2.left, C2.right = F2, G2
E2.right=H2
F2.left=H3

# Initialize nodes
A3 = Node("A")
B3 = Node("B")
C3 = Node("C")
D3 = Node("D")
E3 = Node("E")

# Assign values and edge costs
A3.vl =  4
B3.vl =  8
C3.vl =  6
D3.vl =  2


# Connect nodes (skewed like a linked list)
A3.left = B3
B3.left = C3
C3.left = D3
D3.left = E3



print("This is a demonstration of minmax algorithm")
winner, path = minmax("MAX", A_Node)
print("Example 1 path:", " -> ".join(path))

winner, path = minmax("MAX", A2)
print("Example 2 path:", " -> ".join(path))

winner, path = minmax("MAX", A3)
print("Example 3 path:", " -> ".join(path))