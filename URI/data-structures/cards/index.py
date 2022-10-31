while True:
  length = int(input())
  if length == 0:
    break

  head = 0
  discarded = []
  cards = [x + 1 for x in range(length)]
  while length > 1:
    discarded.append(cards.pop(head))
    length -= 1
    head = (head + 1) % length
  print('Discarded cards: ', end="")
  for (idx, item) in enumerate(discarded):
    print(item, end=", " if idx != len(discarded) - 1 else "\n")
  print(f'Remaining card: {cards[0]}')