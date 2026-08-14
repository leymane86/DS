#Eli Leyman
#CS 580
class Node:
    def __init__(self, d):
        self.data = d
        self.depth = 0
        self.left = None
        self.right = None
        
def IDSsearch(firstNode, goal, depth_limit):
    frontier = [firstNode]
    path = []
    cutoff_occurred = False
    while frontier:
        curr = frontier.pop()
        path.append(curr)
        #print("Visiting:", curr.data)
        #print("Current path:", [n.data for n in path])

        # Check goal
        if curr.data == goal:
            #return [n.data for n in path]
            print("goal path:", [n.data for n in path])
            return(curr)

        # If within depth limit, expand children
        if len(path) < depth_limit:
            if curr.right:
                if(curr.right not in path):
                    frontier.append(curr.right)
                else:
                    print ("Current path:", [n.data for n in path]," contains cycle")
            if curr.left:
                if(curr.left not in path):
                    frontier.append(curr.left)
                else:
                    print ("Current path:", [n.data for n in path]," contains cycle")
        else:
        # we hit depth bound for this node
            cutoff_occurred = True  
        # --- Backtracking step ---
        # While top of path has no children left in frontier pop it
        while path:
            node = path[-1]
            children = [c for c in (node.left, node.right) if c]
            if any(child in frontier for child in children):
                break  # dont backtrack yet
            else:
                path.pop()

    if cutoff_occurred:
        return "cutoff"
    else:
        return "failure"
     
# Initialize and allocate memory for tree nodes
firstNode = Node(1)
secondNode = Node(2)
thirdNode = Node(3)
fourthNode = Node(4)
fifthNode = Node(5)
sixthNode = Node(6)
seventhNode = Node(7)
eighthNode = Node(8)
ninethNode = Node(9)
tenthNode = Node(10)

# Connect binary tree nodes
firstNode.left = secondNode
firstNode.right = thirdNode
secondNode.left = fourthNode
secondNode.right = fifthNode
thirdNode.left = sixthNode
thirdNode.right = seventhNode
fourthNode.left = eighthNode
fourthNode.right = ninethNode
fifthNode.left = tenthNode
#       1
#   2      3
# 4    5   6 7 
#8 9 10           

#case = 1, single goal without cycle, default
#case = 2, multiple goals without cycle
#case = 3, single goal with cycle

depth = 4
goal = 10
i=1
#for i in range (depth):
print("structure of the graph")
print("           1")
print("      2        3")
print("  4      5   6   7 ")
print(" 8 9   10 ")
print("")
print("Goal is to find node value "+str(goal))
print("1 - single goal without cycle, default ")
print("2 - multiple goals without cycle")
print("3 - single goal with cycle")

case = input("Enter your choice: ")
if(case==2):
    print("4th node is updated to 7")
elif(case==3):
    print("cycle is added to node 3")
while(True):    
    frontier = []
    frontier.append(firstNode)
    if(case==2):
        fourthNode.data=goal #prints multiple paths. second path will always be 1,2, goal
    elif(case==3):
        thirdNode.left=firstNode #prints location of cycle
    result = IDSsearch(firstNode,goal,i)
    if(result=="cutoff"):
        print("cutoff")
    elif(result=="failure"):
        print("failure")
        break
    else:
        print("goal found")
        break
    i=i+1
        
