-- Utils.
function GetInput()
  local lines = {}

  for line in io.lines('input.txt') do
    lines[#lines + 1] = line
  end

  return lines
end

-- Part 1
function GetRowCol(line)
  local row = BinarySearch(line:sub(1, 7), 0, 127, 'F', 'B')
  local col = BinarySearch(line:sub(8, 10), 0, 7, 'L', 'R')
  return row, col
end

function BinarySearch(line, l, u, lc, lu)
  for i = 1, #line do
    local v = line:sub(i, i)
    local d = math.ceil((u - l) / 2)

    if (v == lc) then
      u = u - d
    elseif (v == lu) then
      l = l + d
    end
  end

  if (line:sub(#line) == lc) then
    return l
  else
    return u
  end
end

function SeatID(row, col)
  return row * 8 + col
end

-- for part 2
passes = {}
for i = 1, 128 * 8 do
  passes[#passes + 1] = false
end

local lines = GetInput()
local max_seat_id = 0
for i = 1, #lines do
  local row, col = GetRowCol(lines[i])
  local seat_id = SeatID(row, col)
  print(seat_id)
  max_seat_id = math.max(max_seat_id, seat_id)

  passes[seat_id + 1] = true
end

print("Part 1:", max_seat_id)

for i = 1, #passes do
  local seat_id = i - 1
  if (not passes[seat_id] and passes[seat_id - 1] and passes[seat_id + 1]) then
    print("Part 2:", seat_id - 1)
    break
  end
end
