def whoIsBigger(a, b, key, ord = 'desc'):
  if a[key] > b[key]:
    return 'left' if ord == 'desc' else 'right'
  elif b[key] > a[key]:
    return 'right' if ord == 'desc' else 'left'
  else:
    if key == 'gold':
      return whoIsBigger(a, b, 'silver')
    elif key == 'silver':
      return whoIsBigger(a, b, 'bronze')
    elif key == 'bronze':
      return whoIsBigger(a, b, 'name', 'asc')

def mergeSort(arr):
  if len(arr) > 1:
    middle = len(arr)//2
    left = arr[:middle]
    right = arr[middle:]
    mergeSort(left)
    mergeSort(right)

    i = j = k = 0
    while i < len(left) and j < len(right):
      result = whoIsBigger(left[i], right[j], 'gold')
      if result == 'left':
        arr[k] = left[i]
        i += 1
      else:
        arr[k] = right[j]
        j += 1
      k += 1
    
    while j < len(right):
      arr[k] = right[j]
      j += 1
      k += 1

    while i < len(left):
      arr[k] = left[i]
      i += 1
      k += 1


countryNumber = int(input())
classification = []
for i in range(countryNumber):
  entry = input().split()
  country = {
    'name': entry[0],
    'gold': int(entry[1]),
    'silver': int(entry[2]),
    'bronze': int(entry[3])
  }
  classification.append(country)

mergeSort(classification)
for country in classification:
  print(f'{country["name"]} {country["gold"]} {country["silver"]} {country["bronze"]}')