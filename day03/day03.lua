-- Utils.
function get_input()
  local lines = {}

  for line in io.lines('input.txt') do
    lines[#lines + 1] = line
  end

  return lines
end

-- Part 1: count trees we smashed into.
function count_trees(sx, sy, lines)
  local x = 1 -- x position
  local count = 0

  for l = 1, #lines, sy do
    -- check that the cell has a tree or not
    if (lines[l]:sub(x, x) == '#') then
      count = count + 1
    end

    -- move according to the slope
    x = (x + sx - 1) % lines[l]:len() + 1
  end

  return count
end

local lines = get_input()
local slope_3_1 = count_trees(3, 1, lines)
print('Part 1: ', slope_3_1)

-- Part 2: with different slopes
local all_slopes = 1
local slope_1_1 = count_trees(1, 1, lines)
local slope_5_1 = count_trees(5, 1, lines)
local slope_7_1 = count_trees(7, 1, lines)
local slope_1_2 = count_trees(1, 2, lines)
print('Part 2')
print('Slope 1 1: ', slope_1_1)
print('Slope 5 1: ', slope_5_1)
print('Slope 7 1: ', slope_7_1)
print('Slope 1 2: ', slope_1_2)
print('Result: ', slope_1_1 * slope_3_1 * slope_5_1 * slope_7_1 * slope_1_2)
