-- vals.lua
-- This is a configuration file for the Fortran program.
-- Lua isn't too complicated. Check out
-- http://lua-users.org/wiki/TutorialDirectory

-- Parameters
if os.getenv("BATCH") then
   temperature=3.2
else
   temperature=5.0
end

mintemp=3
maxtemp=6
duration=10 -- time to get to 95%
string=10 -- string will be interpreted because that is what this program is looking for
newval=10.5

function pressure(time)
  return mintemp+(maxtemp-mintemp)*math.tanh(1.83*time/duration)
end

