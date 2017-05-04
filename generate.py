from random import sample

N = 4

def up(state):
 for i in range(N):
  for j in range(N):
   if state[i][j] == 0:
    if (i != 0):
     newstate = []
     for I in range(N):
      newstate.append(list(state[I]))
     newstate[i][j] = state[i-1][j]
     newstate[i-1][j] = state[i][j]
     for I in range(N):
      newstate[I] = tuple(newstate[I])
     newstate = tuple(newstate)
     return newstate
 return False


def down(state):
 for i in range(N):
  for j in range(N):
   if state[i][j] == 0:
    if (i != N-1):
     newstate = []
     for I in range(N):
      newstate.append(list(state[I]))
     newstate[i][j] = state[i+1][j]
     newstate[i+1][j] = state[i][j]
     for I in range(N):
      newstate[I] = tuple(newstate[I])
     newstate = tuple(newstate)
     return newstate
 return False

def left(state):
 for i in range(N):
  for j in range(N):
   if state[i][j] == 0:
    if (j != 0):
     newstate = []
     for I in range(N):
      newstate.append(list(state[I]))
     newstate[i][j] = state[i][j-1]
     newstate[i][j-1] = state[i][j]
     for I in range(N):
      newstate[I] = tuple(newstate[I])
     newstate = tuple(newstate)
     return newstate
 return False


def right(state):
 for i in range(N):
  for j in range(N):
   if state[i][j] == 0:
    if (j != N-1):
     newstate = []
     for I in range(N):
      newstate.append(list(state[I]))
     newstate[i][j] = state[i][j+1]
     newstate[i][j+1] = state[i][j]
     for I in range(N):
      newstate[I] = tuple(newstate[I])
     newstate = tuple(newstate)
     return newstate
 return False

def op(state, n):
 if (n == 0):
  return up(state)
 elif (n == 1):
  return down(state)
 elif (n == 2):
  return left(state)
 elif (n == 3):
  return right(state)

def printstate(state):
  r = 0
  s = "BoardState(Board(" + str(N) +", Map( "
  pos = ""
  for row in state:
    c = 0
    for col in row:
      if state[r][c] != 0:
        s += "(" + str(r+1) + ", " + str(c+1) + ") -> " + str(state[r][c]) + ", "
        c += 1
      else:
        pos = "(" + str(r+1) + ", "  + str(c+1) + ")"
        c += 1
    r += 1
  s = s[:-2] + ")), " + pos + ")"
  print s


if __name__ == '__main__':
 state = ((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12), (13, 14, 15, 0))
 states = [set([])]
 states[0].add(state)
 for i in range(1, 21):
  states.append(set([]))
  for State in states[i-1]:
   for Op in range(4):
    a = op(State, Op)
    if (a != False):
     t = True
     for j in range(i):
      if a in states[j]:
       t = False
     if t:
      states[i].add(a)
 for i in range(21):
  print "level " + str(i)
  if len(states[i]) < 25:
    for state in states[i]:
      #print state
      printstate(state)
  else:
    a = sample(states[i], 25)
    for state in a:
      #print state
      printstate(state)

  print "\n"