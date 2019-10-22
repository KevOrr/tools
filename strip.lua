local args = {...}

-- minimum fuel level before returning to refuel
local MINIMUM_FUEL = 2048

if not #args == 0 then
  print("Usage: strip")
  return
end

function main()
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
    unloadOres()
    loadResources()
    
    -- go to next strip entrance
    turtle.turnRight()
    turtle.turnRight()
    for i=1,distanceToChest+5 do
      if turtle.detect() then
        turtle.dig()
      end
      turtle.forward()
      if turtle.detectUp() then
        turtle.digUp()
      end
    end

    -- place a torch
    turtle.turnRight()
    turtle.placeTorch{above=true}
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

function strip()
  local depth = 0
  while shouldContinue() do
    depth = depth + 1

    turtle.dig()
    turtle.forward()
    turtle.digUp()
    turtle.turnLeft()
    turtle.dig()
    turtle.up()
    turtle.dig()
    turtle.turnRight()
    turtle.turnRight()
    turtle.dig()

    if depth % 8 == 0 then
      placeTorch{above=false}
    end

    turtle.down()
    turtle.dig()
    turtle.turnLeft()
    end

  -- return to entrance of strip
  turtle.turnRight()
  turtle.turnRight()
  while turtle.forward() do end
end

function unloadOres()
  for i=1,16 do
    turtle.select(i)
    local detail = turtle.getItemDetail()
    if detail and detail.name ~= "minecraft:torch" and detail.name ~= "minecraft:charcoal" then
      turtle.drop()
    end
  end
end

function loadResources()
  local idx = findItem("minecraft:torches")
  if idx >= 1 then
    turtle.select(i)
    local count = turtle.getItemCount()
    turtle.suckUp(math.max(64 - count, 0))
  end
  
  idx = findItem("minecraft:charcoal")
  if idx >= 1 then
    turtle.select(i)
    local count = turtle.getItemCount()
    turtle.suck(math.max(64 - count, 0))
    refuelCharcoal()
  end
end

function findItem(name)
  for i=1,16 do
    data = turtle.getItemDetail(i)
    if data and data.name == name
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
      refuelFrom(i)
    end
  end
end

function shouldContinue()
  return checkFuel() and inventoryAvail() and hasTorches()
end

function checkFuel()
  if turtle.getFuelLevel() == "unlimited" then
    return true
  end
  if turtle.getFuelLevel() <= MINIMUM_FUEL then
    print("Info: Low fuel")
    return refuelCharcoal() or refuelAny()
  end
  return true
end

function inventoryAvail()
  for i=1,14 do
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
