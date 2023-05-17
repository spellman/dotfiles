-- Source: https://stackoverflow.com/a/27028488/11903744
local function serialize_table(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. serialize_table(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

return {
  serialize_table = serialize_table
}
