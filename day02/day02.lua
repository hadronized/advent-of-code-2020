-- Utils.
function get_input()
  local lines = {}

  for line in io.lines('input.txt') do
    lines[#lines + 1] = line
  end

  return lines
end

-- Parse a rule, e.g. N-M C
function parse_rule(line)
  local dash_index, _ = line:find('-')
  local space_index, _ = line:find(' ')
  local colon_index, _ = line:find(':')
  local lower = tonumber(line:sub(0, dash_index - 1))
  local upper = tonumber(line:sub(dash_index + 1, space_index - 1))
  local letter = line:sub(space_index + 1, space_index + 1) -- wtf?
  local pwd = line:sub(colon_index + 2)

  return {
    lower = lower,
    upper = upper,
    letter = letter,
    pwd = pwd
  }
end

-- Get all the rules.
function get_rules(lines)
  local rules = {}
  for i = 1, #lines do
    rules[#rules + 1] = parse_rule(lines[i])
  end

  return rules
end

-- Check a rule is valid.
function is_rule_valid(rule)
  local count = 0
  for i = 1, #rule.pwd do
    local c = rule.pwd:sub(i, i)

    if (c == rule.letter) then
      count = count + 1
    end
  end

  return (count >= rule.lower and count <= rule.upper)
end

local lines = get_input()
local rules = get_rules(lines)
local valid_count = 0

for i = 1, #rules do
  if (is_rule_valid(rules[i])) then
    valid_count = valid_count + 1
  end
end

io.write('Part 1: ', valid_count, ' passwords are valid\n')

function part_2(rule)
  local count = 0

  if (rule.pwd:sub(rule.lower, rule.lower) == rule.letter) then
    count = count + 1
  end

  if (rule.pwd:sub(rule.upper, rule.upper) == rule.letter) then
    count = count + 1
  end

  return count == 1
end

local valid_count_part_2 = 0
for i = 1, #rules do
  if (part_2(rules[i])) then
    valid_count_part_2 = valid_count_part_2 + 1
  end
end

io.write('Part 1: ', valid_count_part_2, ' passwords are valid\n')
