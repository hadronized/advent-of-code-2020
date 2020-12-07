-- Get rules.
function GetRules()
  local bags = {}

  for line in io.lines("input.txt") do
    local contained = {}
    local c, i = line:find('bags contain ')
    local bag_color = line:sub(1, c - 2)
    i = i + 1 -- beginning of listing (<qty> <color> bag[s]{,.})
    local no, _ = line:find('no other bag', i)

    while (no == nil) do
      local sb, _ = line:find(' ', i) -- first space
      local qty = tonumber(line:sub(i, sb - 1)) -- qty is up to first space - 1
      local bb, be = line:find('bag', sb + 1) -- match up to <bag>s…
      local color = line:sub(sb + 1, bb - 2) -- color is up to bag - 1
      local wb, _ = line:find(' ', be + 2) -- start after <bags{.,}>

      contained[#contained + 1] = {
        color = color,
        quantity = qty
      }

      if (wb == nil) then
        break
      else
        i = wb + 1
      end
    end

    bags[bag_color] = contained
  end

  return bags
end

local rls = GetRules()

-- Part 1
function DFS(rules, color, visited)
  if (visited[color] ~= nil) then
    -- we already know the amount for this one; shortcut
    return visited[color]
  else
    -- first time we see this color
    local can_contain = 0

    for _, sub in pairs(rules[color]) do
      if (sub.color == 'shiny gold') then
        can_contain = 1
        break
      else
        if (DFS(rules, sub.color, visited) == 1) then
          can_contain = 1
          break
        end
      end
    end

    visited[color] = can_contain

    return can_contain
  end
end

function Part1(rules)
  local golden_map = {}
  local sum = 0

  for color, bag in pairs(rules) do
    if (color ~= 'shiny gold') then
      sum = sum + DFS(rules, color, golden_map)
    end
  end

  return sum
end

print('Part 1:', Part1(rls))

-- Part 2.
function Part2(rules, color, visited)
  if (visited[color] ~= nil) then
    -- we already know the amount for this one; shortcut
    return visited[color]
  else
    -- first time we see this color
    local sum = 1

    for _, sub in pairs(rules[color]) do
      sum = sum + Part2(rules, sub.color, visited) * sub.quantity
    end

    visited[color] = sum

    return sum
  end
end

print('Part 2:', Part2(rls, 'shiny gold', {}) - 1)
