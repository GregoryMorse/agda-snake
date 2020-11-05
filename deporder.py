import os
import glob
import re
#root = "jAgda.snake"
for path in ("snakejs", "emuterm"):
  graph, pred = {}, {}
  for filePath in glob.glob(os.path.join(".", path, "*.js")):
    fname = os.path.splitext(os.path.basename(filePath))[0]
    if not fname in graph: graph[fname] = set()
    #if not fname in pred: pred[fname] = set()
    with open(filePath, encoding="utf-8") as fp:
      line = fp.readline()
      while line:
        match = re.search("require\(\"([.\w]+)\"\)", line)
        if not match is None:
          graph[fname].add(match[1])
          #if not match[1] in pred: pred[match[1]] = set()
          #pred[match[1]].add(fname)
        line = fp.readline()
  #visited = set()
  #bfs = []
  #level, nextlevel = [x for x in graph if len(graph[x]) == 0], []
  #visited = set(level)
  #while True:
  #	bfs.extend(level)
  #	for x in level:
  #		for z in pred[x]:
  #			if not z in visited and all(q in visited for q in graph[z]): #BFS O(m+n) but this makes it O(m^2+n))!!!
  #				visited.add(z); nextlevel.append(z)
  #	level = nextlevel; nextlevel = []
  #	if len(level) == 0: break
  #for z in bfs:
  #	print("\"" + z + "\", ", end="")
  #print("")
  #https://en.wikipedia.org/wiki/Topological_sorting Depth-first search linear time O(m+n)
  visited = set()
  postorder = []
  def visit(n):
    if n in visited: return
    visited.add(n)
    for m in graph[n]: visit(m)
    postorder.append(n)
  for x in graph: visit(x)
  for z in postorder:
    print("\"" + z + "\", ", end="")
  print("")