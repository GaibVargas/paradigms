from math import floor

iteration_number = int(input())

for i in range(iteration_number):
  line = input()
  move_three = ''
  for letter in line:
    if letter.isalpha():
      move_three += chr(ord(letter) + 3)
    else:
      move_three += letter
  reverse = move_three[::-1]
  result = reverse[:floor(len(reverse)/2)]
  for x in range(floor(len(reverse)/2), len(reverse)):
    result += chr(ord(reverse[x]) - 1)
  print(result)