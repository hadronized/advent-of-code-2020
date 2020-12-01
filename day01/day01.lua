-- Utils.
function get_input()
  local lines = {}

  for line in io.lines('input.txt') do
    lines[#lines + 1] = tonumber(line)
  end

  return lines
end

local lines = get_input()

-- Part 1.
function part_1(lines)
  for i = 1, #lines - 1 do
    for j = i, #lines do
      if (lines[i] + lines[j]) == 2020 then
        io.write('Part 1: ', lines[i] * lines[j], '\n')
        return
      end
    end
  end

  io.write('cannot find an anwser\n')
end

part_1(lines)

-- Part 2.
function part_2(lines)
  for i = 1, #lines - 2 do
    for j = i, #lines - 1 do
      for k = j, #lines do
        if (lines[i] + lines[j] + lines[k]) == 2020 then
          io.write('Part 2: ', lines[i] * lines[j] * lines[k], '\n')
          return
        end
      end
    end
  end

  io.write('cannot find an anwser\n')
end

part_2(lines)
