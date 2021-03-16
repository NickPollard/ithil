function f(x, y)
  local l = function (t, u)
    return u
  end
  return l(y, x)
end
