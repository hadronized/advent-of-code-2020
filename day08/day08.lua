function GetCode()
  local code = {}

  for line in io.lines('input.txt') do
    local sb, _ = line:find(' ')
    code[#code + 1] = {
      operator = line:sub(1, sb - 1),
      operand = tonumber(line:sub(sb + 1))
    }
  end

  return code
end

function ExInstr(ctx, code, i)
  local instr = code[i]
  local operator = instr.operator
  local operand = instr.operand
  local ops = {
    ['nop'] = function()
      return i + 1
    end,

    ['acc'] = function ()
      ctx.global = ctx.global + operand
      return i + 1
    end,

    ['jmp'] = function()
      return i + operand
    end
  }

  return ops[operator]()
end

function Run(ctx, code)
  local seen = {}
  local ip = 1

  for _ = ip, #code do
    if (ip > #code or seen[ip] ~= nil) then
      break
    else
      seen[ip] = {} -- mark this instruction as seen

      ip = ExInstr(ctx, code, ip)
    end
  end

  return ip
end

function Part1(code)
  local ctx = { global = 0 }
  local ip = Run(ctx, code)

  print(string.format('Part 1: %d', ctx.global))
end

function SwitchInstr(instr)
  if (instr == 'nop') then
    return 'jmp'
  elseif (instr == 'jmp') then
    return 'nop'
  else
    return instr
  end
end

function Part2(code)
  for i = 1, #code do
    local operator = code[i].operator
    if (operator == 'nop' or operator == 'jmp') then
      local new_code = code
      new_code[i].operator = SwitchInstr(operator)

      local ctx = { global = 0 }
      local ip = Run(ctx, new_code)
      if (ip == #new_code + 1) then
        print(string.format('Part 2: %d', ctx.global))
        break
      end

      -- reset the instruction so that the next iteratons (if any) are not corrupted by this run
      new_code[i].operator = operator
    end
  end
end

local code = GetCode()
Part1(code)
Part2(code)
