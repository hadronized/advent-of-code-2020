function GetGroups()
  local groups = {}
  local group = {}
  local persons = 0

  for line in io.lines('input.txt') do
    if (#line == 0) then
      -- end of group
      groups[#groups + 1] = { group = group, persons = persons }
      group = {}
      persons = 0
    else
      for i = 1, #line do
        local l = line:sub(i, i)
        if (group[l] == nil) then
          group[l] = 0
        end

        group[l] = group[l] + 1
      end

      persons = persons + 1
    end
  end

  groups[#groups + 1] = { group = group, persons =persons }

  return groups
end

local grps = GetGroups()

-- Part 1
function Part1(groups)
  local sum = 0

  for g = 1, #groups do
    for _, _ in pairs(groups[g].group) do
      sum = sum + 1
    end
  end

  print('Part 1:', sum)
end

Part1(grps)

-- Part2
function Part2(groups)
  local total = 0
  for _ , group in pairs(groups) do
    local count = 0
    for _, l in pairs(group.group) do
      if (l == group.persons) then
        count = count + 1
      end
    end

    total = total + count
  end

  print('Part 2:', total)
end

Part2(grps)
