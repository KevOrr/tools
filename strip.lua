local args = {...}

-- minimum fuel level before returning to refuel
local MINIMUM_FUEL = 2048

if not #args == 0 then
  print("Usage: strip")
  return
end

function main()
  print("Hold Ctrl-T to quit program")
  local distanceToChest = 0
  while true do
    -- strip until out of torches, inventory space, or fuel, then return
    strip()

    -- go back to chest
    turtle.turnRight()
    distanceToChest = 0
    while turtle.forward() do
      distanceToChest = distanceToChest + 1
    end
    loadAndUnload()
    
    -- go to next strip entrance
    turtle.turnRight()
    turtle.turnRight()
    for i=1,distanceToChest+6 do
      dig()
      turtle.forward()
      digUp()
    end
    turtle.back()

    -- place a torch
    turtle.turnRight()
    placeTorch{above=true}
    turtle.turnLeft()
    turtle.turnLeft()
  end
end

function placeTorch(opts)
  if type(opts.above) ~= "boolean" then
    opts.above = false
  end

  local idx = findItem("minecraft:torch")
  if idx >= 1 then
    turtle.select(idx)
    if opts.above then
      return turtle.placeUp()
    else
      return turtle.place()
    end
  end
  return false
end

function dig()
  while turtle.detect() do
    turtle.dig()
    sleep(0.3)
  end
end

function digUp()
  local _, detail = turtle.inspectUp()
  while turtle.detectUp() and detail.name ~= "minecraft:torch" do
    turtle.digUp()
    sleep(0.3)
    _, detail = turtle.inspectUp()
  end
end

function strip()
  local depth = 0
  while shouldContinue() do
    depth = depth + 1
    turtle.select(1)
    print(string.format("INFO: fuel level: %s", tostring(turtle.getFuelLevel())))

    dig()
    while not turtle.forward() do end
    digUp()
    turtle.turnLeft()
    dig()
    while not turtle.up() do end
    dig()
    turtle.turnRight()
    turtle.turnRight()
    dig()

    if depth % 8 == 0 then
      placeTorch{above=false}
    end

    while not turtle.down() do end
    dig()
    turtle.turnLeft()
  end

  -- return to entrance of strip
  turtle.turnRight()
  turtle.turnRight()
  while turtle.forward() do end
end

function loadAndUnload()
  for i=1,16 do
    turtle.select(i)
    local detail = turtle.getItemDetail()
    if detail ~= nil and detail.name ~= "minecraft:torch" and detail.name ~= "minecraft:charcoal" then
      turtle.dropDown()
    end
  end

  while not hasTorches() do
    local idx = findItem("minecraft:torches")
    if idx >= 1 then
      turtle.select(i)
      local count = turtle.getItemCount()
      turtle.suckUp(math.max(64 - count, 0))
    else
      turtle.suckUp(64)
    end
  end

  while not checkFuel() do
    for i=1,16 do
      if turtle.getItemCount(i) == 0 then
        turtle.select(i)
        break
      end
    end
    turtle.suck(1)
    turtle.refuel()
    if turtle.getItemCount() ~= 0 then
      turtle.dropDown()
    end
  end
end

function findItem(name)
  for i=1,16 do
    data = turtle.getItemDetail(i)
    if data and data.name == name then
      return i
    end
  end
  return -1
end

function refuelFrom(slot)
  turtle.select(slot)
  local fueled = false
  while turtle.getFuelLevel() < turtle.getFuelLimit() and turtle.refuel(1) do
    fueled = true
  end
  return fueled
end

function refuelCharcoal()
  local idx = findItem("minecraft:charcoal")
  if idx < 1 then
    return false
  end
  return refuelFrom(idx)
end

function refuelAny()
  for i=1,16 do
    turtle.select(i)
    if turtle.refuel(0) then
      print(string.format("Warning: refueling with %s", turtle.getItemDetail().name))
      return refuelFrom(i)
    end
  end
end

function shouldContinue()
  return checkFuel() and inventoryAvail() and hasTorches()
end

function checkFuel()
  return turtle.getFuelLevel() == "unlimited" or turtle.getFuelLevel() > MINIMUM_FUEL
end

function inventoryAvail()
  for i=1,16 do
    local detail = turtle.getItemDetail(i)
    if detail ~= nil and (detail.name == "minecraft:cobblestone" or detail.name == "minecraft:gravel") then
      turtle.select(i)
      turtle.drop()
    end
      
    if turtle.getItemCount(i) == 0 then
      return true
    end
  end
  return false
end

function hasTorches()
  return findItem("minecraft:torch") >= 1
end

main()
