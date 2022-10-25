-- Remove all HTML-Blocks containing the class "lua-remove"
function Div (div)
  if div.classes:includes('lua-remove') then
    return {}
  end
  return nil
end