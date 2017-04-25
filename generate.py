N = 3

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

if __name__ == '__main__':
 state = ((1, 2, 3), (4, 5, 6), (7, 8, 0))
 states = [set([])]
 states[0].add(state)
 for i in range(1, 20):
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
 for i in range(20):
  print "level " + str(i)
  for state in states[i]:
   print state

  print "\n\n"