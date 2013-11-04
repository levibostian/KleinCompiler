def ABS( n )
  if n < 0
    -n
  else
    n
  end
end

def f( x, n )
  (x * x) - n
end

def df( x )
  return 2 * x
end

def newtonAux( guess, previous, epsilon, n )
  if epsilon < ABS(previous-guess)
    newtonAux( guess - f(guess, n)/df(guess), guess, epsilon, n )
  else
    guess
  end
end

def newton( guess, epsilon, n )
  newtonAux( guess - f(guess,n)/df(guess), guess, epsilon, n )
end

def printAndReturn( answer )
  p answer
  answer
end

n = ARGV[0].to_i
epsilon = ARGV[1].to_i
printAndReturn( newton(n/2, epsilon, n) )

