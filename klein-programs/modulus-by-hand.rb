def mod(m, n)
  if m < n then
     m
  else
     mod(m-n, n)
  end
end

m = ARGV[0].to_i
n = ARGV[1].to_i
p m / n
p mod(m,n)
# mod(m,n)
