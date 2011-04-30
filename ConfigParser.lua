-- This module loads a lua file as a configuration file.
module(...,package.seeall)


-- loadfile on the filename but read its values into a table.
function open(filename)
  paramtable={}
  setmetatable(paramtable,{__index=_G})
  f=loadfile(filename)
  setfenv(f,paramtable)
  f()
  --setmetatable(paramtable,nil)
  return paramtable
end


