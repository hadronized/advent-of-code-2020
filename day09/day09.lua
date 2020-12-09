local COUNT = 25

function GetNumbers()
  local numbers = {}

  for line in io.lines('input.txt') do
    numbers[#numbers + 1] = tonumber(line)
  end

  return numbers
end

-- Ce langage est une putain de blague.
function tlen(t)
  local n = 0

  for _, _ in pairs(t) do
    n = n + 1
  end

  return n
end

function ComputeSums(numbers, sum_map, a, i, z)
  for j = z, z + COUNT - 1 do
    local b = numbers[j]

    if (a ~= b) then
      local sum = a + b

      if (sum_map[sum] == nil) then
        sum_map[sum] = { [i] = {}, [j] = {} }
      else
        -- there’s already at least one sum; add ours to the set of sums
        sum_map[sum][i] = {}
        sum_map[sum][j] = {}
      end
    end
  end
end

-- Part 1
--
-- First, we compute a reverse sum hashmap (sum -> pairs) for the preamble. Then, starting from the ith (26th) number,
-- we look into the hashmap: if the number has associated pairs, it means that there exist at least one pair of numbers
-- summing to this number. If there’s none, here’s our number.
--
-- In the case of there is at least one pair, we go to the next number. We look into the hashmap for the number that was
-- evicted (i.e. 1st number in this case) and if a pair has it, we remove it from the hashmap value. If we end up with an
-- empty object, we remove its associated key. Then, the previous number now becomes part of the sum, so we simply run a
-- single pass trying to add it again to everything.
function Part1(numbers)
  local sum_map = {}

  -- Precompute the preamble.
  for i = 1, COUNT - 1 do
    local a = numbers[i]

    ComputeSums(numbers, sum_map, a, i, i + 1)
  end

  -- Iterate through all remaining numbers
  for i = COUNT + 1, #numbers do
    local n = numbers[i]

    if (sum_map[n] == nil) then
      return n
    end

    -- update the sum map to evict all pairs containing the number to evict
    local to_evict = numbers[i - COUNT]
    for _, sum_n in pairs(sum_map) do
      sum_n[to_evict] = nil

      -- if the sum has no associated numbers anymore, remove the sum
      if (tlen(sum_n) == 0) then
        sum_n = nil
      end
    end

    -- take into account the new number for creating new sums
    ComputeSums(numbers, sum_map, n, i, i - COUNT)
  end

  return nil
end

function MinMax(t, s, e)
  local min, max = t[s], t[s]

  for i = s, e do
    local n = t[i]
    if n < min then
      min = n
    elseif n > max then
      max = n
    end
  end

  return min, max
end

-- Part 2
function Part2(numbers, invalid)
  local memo = {}

  for i = 1, #numbers do
    if (memo[i] == nil) then
      memo[i] = {}
    end

    memo[i][1] = numbers[i]
  end

  for size = 2, #numbers do
    print(string.format('size = %d', size))
    for i = 1, #numbers - size do
      local sum = numbers[i] + memo[i + 1][size - 1]

      if (sum == invalid) then
        return i, i + size - 1
      end

      memo[i][size] = sum
    end
  end

  return 0, 0
end

local numbers = GetNumbers()

local part_1 = Part1(numbers)
print(string.format('Part 1: %d', part_1))

local i, j = Part2(numbers, part_1)
local min, max = MinMax(numbers, i, j)
print(string.format('Part 2: %d + %d = %d', min, max, min + max))
