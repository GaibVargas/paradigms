inputs = int(input())

for i in range(inputs):
  stack = []
  diamods_number = 0
  line = input()
  for char in line:
    if char == '<':
      stack.append(char)
    if char == '>' and len(stack) > 0:
      stack.pop()
      diamods_number += 1
  print(diamods_number)
