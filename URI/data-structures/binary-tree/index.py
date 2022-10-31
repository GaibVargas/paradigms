class Node:
  def __init__(self, value):
    self.value = value
    self.right = None
    self.left = None
  
  def add(self, node):
    if node.value > self.value:
      if self.right == None:
        self.right = node
        return
      self.right.add(node)
    elif node.value < self.value:
      if self.left == None:
        self.left = node
        return
      self.left.add(node)

class Tree:
  def __init__(self):
    self.root = None
  
  def add(self, number):
    node = Node(number)
    if self.root == None:
      self.root = node
    else:
      self.root.add(node)

class QueueNode:
  def __init__(self, node):
    self.value = node
    self.next = None

class Queue:
  def __init__(self, node):
    self.len = 1
    item = QueueNode(node)
    self.head = item
    self.tail = self.head
  
  def add(self, node):
    item = QueueNode(node)
    if self.head == None:
      self.head = item
      self.tail = item
    else:
      self.tail.next = item
      self.tail = item
    self.len += 1
  
  def pop(self):
    item = self.head
    self.head = self.head.next
    self.len -= 1
    return item

entries = int(input())
for i in range(entries):
  n = int(input())
  numbers = [int(x) for x in input().split()]
  tree = Tree()
  for number in numbers:
    tree.add(number)
  print(f"Case {i + 1}:")
  queue = Queue(tree.root)
  while queue.len > 0:
    item = queue.pop()
    if (item.value.left != None):
      queue.add(item.value.left)
    if (item.value.right != None):
      queue.add(item.value.right)
    print(f"{item.value.value}", end=" " if queue.len > 0 else "\n")
  print()
