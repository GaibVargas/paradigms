inputs_number = int(input())

for i in range(inputs_number):
  [entries, n_entries] = [int(x) for x in input().split()]
  table = []
  for n in range(entries):
    table.append([])
  numbers = [int(x) for x in input().split()]
  for number in numbers:
    table[number % entries].append(number)
  for (idx, array) in enumerate(table):
    print(f'{idx} -> ', end="")
    for number in array:
      print(f'{number} -> ', end="")
    print('\\')
  if i != inputs_number - 1:
    print()