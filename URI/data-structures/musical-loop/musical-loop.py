while True:
  length = int(input())
  if length == 0:
    break

  inputs = input().split()
  points = []
  for i in inputs:
    points.append(int(i))
  
  peaks_number = 0
  for (idx, item) in enumerate(points):
    if idx == 0:
      if (item < points[-1] and item < points[idx + 1]) or (item > points[-1] and item > points[idx + 1]):
        peaks_number += 1
    elif idx == length - 1:
      if (item < points[-2] and item < points[0]) or (item > points[-2] and item > points[0]):
        peaks_number += 1
    elif (item < points[idx - 1] and item < points[idx + 1]) or (item > points[idx -1] and item > points[idx + 1]):
      peaks_number += 1
  
  print(peaks_number)