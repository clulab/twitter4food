src = 'src/main/resources/org/clulab/twitter4food/'
pkg = 'featureclassifier/gender/'
filename = ['results-u.txt', 'results-b.txt', 'results-ub.txt']
files = map(lambda x: src+pkg+x, filename)

class datum:
  def __init__(self, lbl, c, k, p, r, f, a, avg):
    self.lbl = lbl
    self.c = c
    self.k = k
    self.p = p
    self.r = r
    self.f = f
    self.a = a
    self.avg = avg

for file in files:
  fin = open(file, 'r')
  lines = []
  c_iters = range(0,7)
  k_iters = range(0,7)

  data = []

  for line in fin:
    lines.append(line)

  for c in c_iters:
    c_lines = lines[c*35:(c+1)*35]
    for k in k_iters:
      k_lines = lines[k*5:(k+1)*5]
      _0 = k_lines[0]
      _1 = k_lines[1]
      _2 = k_lines[2]
      _3 = k_lines[3]
      _4 = k_lines[4]

      _c = float(_0[_0.index('=')+1:_0.index(',')])
      _k = int(_0[_0.rindex('=')+1:])

      splits = _1.split("\t")
      _lbl1 = splits[0][0]
      _p1 = float(splits[0][splits[0].index(':')+2:])
      _r1 = float(splits[1][splits[1].index(':')+2:])
      _f1 = float(splits[2][splits[2].index(':')+2:])

      splits2 = _2.split("\t")
      _lbl2 = splits2[0][0]
      _p2 = float(splits[0][splits[0].index(':')+2:])
      _r2 = float(splits[1][splits[1].index(':')+2:])
      _f2 = float(splits[2][splits[2].index(':')+2:])

      _a = float(_3[_3.index(':')+2:])
      _avg = float(_4[_4.index(':')+2:])

      data.append(datum(_lbl1, _c, _k, _p1, _r1, _f1, _a, _avg))
      data.append(datum(_lbl2, _c, _k, _p2, _r2, _f2, _a, _avg))

  print len(data)