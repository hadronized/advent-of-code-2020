-- Utils.
function get_documents()
  local documents = {}
  local document = {}

  for line in io.lines('input.txt') do
    if (line:len() == 0) then
      documents[#documents + 1] = document
      document = {}
    end

    local i = 1

    while true do
      local pair = ''
      pair, i = extract_pair(line, i)

      if (pair == nil) then
        break
      end

      local colon = pair:find(':')
      local key = pair:sub(1, colon - 1)
      local value = pair:sub(colon + 1)

      document[key] = value
    end

  end

  documents[#documents + 1] = document
  return documents
end

-- Extract a pair from a line, and return the next index at which continue searching.
function extract_pair(line, s)
  local i, j = line:find('...:[#%w]+', s)

  if (i == nil) then
    return nil
  end

  return line:sub(i, j), j
end

local documents = get_documents()

print(#documents, " documents")

-- Part 1
function is_present(document)
  return document.byr ~= nil and document.iyr ~= nil and document.eyr ~= nil and document.hgt ~= nil
    and document.hcl ~= nil and document.ecl ~= nil and document.pid ~= nil
end

local part_1 = 0
for i = 1, #documents do
  if (is_present(documents[i])) then
    part_1 = part_1 + 1
  end
end

print('Part 1: ', part_1)

-- Part 2
function is_valid(document)
  return check_year(document.byr, 1920, 2002)
    and check_year(document.iyr, 2010, 2020)
    and check_year(document.eyr, 2020, 2030)
    and check_height(document.hgt)
    and document.hcl:match('^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$') ~= nil
    and check_eye_color(document.ecl)
    and document.pid:match('^%d%d%d%d%d%d%d%d%d$') ~= nil
end

function check_year(s, min, max)
  local m = s:match('%d%d%d%d')

  if (m == nil) then
    return false
  end

  local n = tonumber(m)

  return min <= n and n <= max
end

function check_height(s)
  local m, i = s:find('^%d+')

  if (m == nil) then
    return false
  end

  local height = tonumber(s:sub(1, i))

  local unit = s:sub(i + 1)

  if (unit == 'cm') then
    return 150 <= height and height <= 193
  elseif (unit == 'in') then
    return 59 <= height and height <= 76
  else
    return false
  end
end

function check_eye_color(s)
  local colors = {}
  colors['amb'] = true
  colors['blu'] = true
  colors['brn'] = true
  colors['gry'] = true
  colors['grn'] = true
  colors['hzl'] = true
  colors['oth'] = true

  return colors[s] ~= nil
end

local part_2 = 0
for i = 1, #documents do
  local document = documents[i]
  if (is_present(document) and is_valid(document)) then
    part_2 = part_2 + 1
  end
end

print('Part 2: ', part_2)
