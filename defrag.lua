--[[
	Applied Energistics 2 Storage defragmentation
	Version: 0.2
	Author: feryaz
]]--
-- Some waiting times are just to show a message on the monitor, set to false for faster fragmenting; wont change import and export speeds
local ambientSleeps = 0.1

-- Wrap drive of buffer network
local bufferDrive = peripheral.wrap("appliedenergistics2:drive_7")

-- Wrap monitor
local mon = peripheral.wrap("right")

-- Wrap OpenPeripheralSelector to display item icon, set to 'false' if you dont have it
local selector = peripheral.wrap("top")

-- Wrap interface of buffer network
local bufferInterface = peripheral.wrap("left")

-- Cardinal direction from main network interface to buffer network interface (UP/DOWN/NORTH/EAST/SOUTH/WEST)
local bufferInterfaceDirection = "UP"

-- Wrap interface of main network
local interface = peripheral.wrap("appliedenergistics2:interface_6")

-- Cardinal direction from buffer network interface to main network interface
local interfaceDirection = "DOWN"

-- The cardinal directions that the left and right sides of the drives are facing (look right/left while standing in front of the drives and hit F3)
local drivesDirectionLeft = "SOUTH"
local drivesDirectionRight = "NORTH"

-- Array of drives in main network
-- Each drive must face another one to left or right
-- Drive [11] must be defined, set to priority 1, be empty, have a drive in each row below/above it
-- Every other drive must have their first slot empty
-- You can replace drives with empty chests if you want to space them out
-- The [key] is made up of x and y as of how the drives are setup where x is the row (beginning 0 in the left) and y is the column from up to down (beginning 0 at the top)
local drives = {
	[11] = peripheral.wrap("bottom"),
	[12] = peripheral.wrap("appliedenergistics2:drive_1"),
	[13] = peripheral.wrap("appliedenergistics2:drive_2"),
	[20] = peripheral.wrap("appliedenergistics2:drive_6"),
	[21] = peripheral.wrap("appliedenergistics2:drive_5"),
	[22] = peripheral.wrap("appliedenergistics2:drive_4"),
	[23] = peripheral.wrap("appliedenergistics2:drive_3"),
}

------- End of Config ---------

local w, h = mon.getSize()
local finishedItems = {}
local finishedItemsCount = 0
local cellOrigin = {}
local totalItemsCount = 0
local itemName = 0
local inDrives = 0
local inBuffer = 0

--display text text on monitor, "mon" peripheral
function drawText(x, y, text, text_color, bg_color)
  mon.setBackgroundColor(bg_color)
  mon.setTextColor(text_color)
  mon.setCursorPos(x,y)
  mon.write(text)
end
 
--draw line on monitor
function drawLine(x, y, length, color)
    mon.setBackgroundColor(color)
    mon.setCursorPos(x,y)
    mon.write(string.rep(" ", length))
end

--clear lines on monitor
function clearLine(...)
    for _,y in ipairs(arg) do
	    mon.setCursorPos(1,y)
		mon.clearLine()
	end
end

function updateTask(msg, color, percent)
	percent = math.floor(percent)
	mon.setBackgroundColor(colors.black)
	clearLine(6,7,8,9,10)
	
	drawText(2, 6, "Task", colors.white, colors.black)
	
	drawText(math.floor(w - string.len(percent.."%")), 6, percent.."%", colors.white, colors.black)

	drawText(2, 7, msg, color, colors.black)
	
	if itemName ~= 0 then
		drawText(2, 9, itemName, colors.white, colors.black)

		drawText(2, 10, ""..inDrives, colors.lightGray, colors.black)
		drawText(math.floor(w-string.len(""..inBuffer)), 10, ""..inBuffer, colors.lightGray, colors.black)
		
		color = colors.orange
		
		if percent >= 50 then
			color = colors.blue
		end
		
		percent = math.floor(100/(inDrives+inBuffer)*inBuffer)
		local barLength = w-2
		local barBreak = math.ceil(barLength/100*percent)
		drawLine(2, 11, barBreak, color)
		drawLine(2+barBreak, 11, w-barBreak-2, colors.gray)
	end
end

function updateProgress()
	mon.setBackgroundColor(colors.black)
	mon.clear()
	drawText(2, 2, "Progress ", colors.white, colors.black)
	local percent = math.floor(100/totalItemsCount*finishedItemsCount)
	local percentString = percent.."%"
	drawText(math.floor(w - string.len(percentString)), 2, percentString, colors.white, colors.black)

	drawText(2, 3, finishedItemsCount.."/"..totalItemsCount, colors.lightGray, colors.black)
	
	local barLength = w-2
	local barBreak = math.ceil(barLength/100*percent)
	drawLine(2, 4, barBreak, colors.green)
	drawLine(2+barBreak, 4, w-barBreak-2, colors.gray)
end

function itemKey(item)
  local key = item.id .. "/" .. item.dmg

  if item.nbt_hash ~= nil then
    key = key .. "/" .. item.nbt_hash
  end

  return key
end

function findItemToDefrag()
	local item
	local availableItems = interface.getAvailableItems()
	itemName = 0
	inDrives = 0
	inBuffer = 0

	for _, item in pairs(interface.getAvailableItems()) do 
		if not finishedItems[itemKey(item.fingerprint)] then
			if totalItemsCount == 0 then
				totalItemsCount = #availableItems
				updateProgress()
			end
			updateTask("Find next item", colors.lightGray, 0)
			if ambientSleeps then
				sleep(ambientSleeps)
			end

			return exportItem(item)
		end
	end
	return false
end

function exportItem(item)
	local details = interface.getItemDetail(item.fingerprint).basic();
	itemName = details.display_name
	if selector then
		selector.setSlot(1, item.fingerprint)
	end
	while details and details.qty > 0 do
		inDrives = details.qty
		updateTask("Export to buffer", colors.lightGray, 50/(inDrives+inBuffer)*inBuffer)
		local status = interface.exportItem(item.fingerprint, bufferInterfaceDirection)
		if details.qty < details.max_size then
			inBuffer = inBuffer + details.qty
			inDrives = 0
			details = nil
		else
			inBuffer = inBuffer + details.max_size
			details.qty = details.qty - details.max_size
			inDrives = details.qty
			sleep(0.001)
		end
	end
	return checkItem(item)
end

function importItem(item)
	local details = bufferInterface.getItemDetail(item.fingerprint).basic();
	while details and details.qty > 0 do
		inBuffer = details.qty
		updateTask("Import to cell", colors.lightGray, 50+50/(inDrives+inBuffer)*inDrives)
		if bufferInterface.exportItem(item.fingerprint, interfaceDirection) then
			if details.qty < details.max_size then
				inDrives = inDrives + details.qty
				inBuffer = 0
				details = nil
			else
				inDrives = inDrives + details.max_size
				details.qty = details.qty - details.max_size
				if details.qty == 0 then
					inBuffer = 0
				end
				sleep(0.001)
			end
		else
			return false
		end
	end
	return true
end

function checkItem(item)
	local details = bufferInterface.getItemDetail(item.fingerprint)
	while details do
		details = details.all()
		local requiredDrive
		local stacks = math.ceil(details.qty/64)
		updateTask("Find best Cell", colors.lightGray, 50)

		if ambientSleeps then
			sleep(ambientSleeps)
		end

		if stacks > 16 then
			requiredDrive = 65536
		elseif stacks > 4 then
			requiredDrive = 16384
		elseif stacks > 1 then
			requiredDrive = 4096
		else
			requiredDrive = 1024
		end

		if findDrive(requiredDrive, details.qty) then
			if importItem(item) then
				finishedItems[itemKey(item.fingerprint)] = 1
				finishedItemsCount = finishedItemsCount + 1
				
				updateProgress()
				updateTask("Finished", colors.lime, 100)
				
				if ambientSleeps then
					os.sleep(ambientSleeps)
				end
				
				details = nil
				return true
			else
				details = bufferInterface.getItemDetail(item.fingerprint)
			end
		else
			print("Please insert new "..requiredDrive.."Bytes drive and wait ~20 sec")
			updateTask("Insert new "..requiredDrive.."B drive", colors.red, 50)
			sleep(20)
			details = bufferInterface.getItemDetail(item.fingerprint)
		end
	end
	-- Wait for item to appear in buffer
	print("Wait for item to appear in buffer "..item.fingerprint.id)
	sleep(0.01)

	return checkItem(item)
end

function findDrive(requiredDrive, amount)
	local bytes = amount/8 + (requiredDrive/128)
	local name, drive, slot
	for name, drive in pairs(drives) do
		for slot = 1, 10, 1 do
			local cell = drive.getStackInSlot(slot)
			if cell then
				if  cell.me_cell.totalBytes == requiredDrive and 
					(cell.me_cell.freeBytes > bytes or cell.me_cell.freeBytes == cell.me_cell.totalBytes)
					and cell.me_cell.freeTypes > 0 
					and not cell.me_cell.preformatted 
				then
					if name == 11 and slot == 1 then
						-- best drive alreay in position
						return true
					elseif freeFirstSlot() then
						cellOrigin = {name, slot}
						updateTask("Get cell", colors.lightGray, 50)
						if ambientSleeps then
							sleep(ambientSleeps)
						end

						return moveCell(""..name, slot, "11", 1)
					end
				end
			end
		end
	end
	return false
end

function freeFirstSlot()
	if #cellOrigin > 0 then
		updateTask("Remove cell", colors.lightGray, 50)
		if ambientSleeps then
			sleep(ambientSleeps)
		end

		if moveCell("11", 1, cellOrigin[1], cellOrigin[2], true) then
			cellOrigin = {}
			return true
		end

		return false
	end
	return true
end

function moveCell(from, fromSlot, to, toSlot, back)
	local move = true
	local moved = false
	local drive, fromY, fromX, toY, toX
	
	while move do
		drive = drives[tonumber(from)]
		fromY = tonumber(string.sub(from, 1, 1))
		toY = tonumber(string.sub(to, 1, 1))
		
		fromX = tonumber (string.sub(from, 2, 2))
		toX = tonumber( string.sub(to, 2, 2) )

		-- directly insert in right slot if next drive will be destination as swapStack doesn't seem to work
		insertIntoSlot = 1
		if math.abs(fromX-toX) + math.abs(fromY-toY) == 1 then
			insertIntoSlot = toSlot
		end
		if fromX == toX or back then
			if fromY == toY then
				if back then 
					back = false
				else
					if fromSlot == toSlot then
						moved = true
						move = false
					else
						-- go right to come back in correct slot as swapStack does not work; should not happen
						if drive.pushItemIntoSlot(drivesDirectionRight, fromSlot, 1, insertIntoSlot) == 1 then
							from = fromY..""..(fromX+1)
							fromSlot = insertIntoSlot
						end
					end
				end
			else
				if fromY > toY then
					if drive.pushItemIntoSlot("UP", fromSlot, 1, insertIntoSlot) == 1 then
						from = (fromY-1)..""..fromX
						fromSlot = insertIntoSlot
					end
				else
					if drive.pushItemIntoSlot("DOWN", fromSlot, 1, insertIntoSlot) == 1 then
						from = (fromY+1)..""..fromX
						fromSlot = insertIntoSlot
					end
				end

			end
		else
			if fromX > toX then
				if drive.pushItemIntoSlot(drivesDirectionLeft, fromSlot, 1, insertIntoSlot) == 1 then
					from = fromY..""..(fromX-1)
					fromSlot = insertIntoSlot
				end
			else
				if drive.pushItemIntoSlot(drivesDirectionRight, fromSlot, 1, insertIntoSlot) == 1 then
					from = fromY..""..(fromX+1)
					fromSlot = insertIntoSlot
				end
			end
		end
	end
	return moved
end

mon.setTextScale(1)
mon.setBackgroundColor(colors.black)
mon.clear()

while findItemToDefrag() do

end

freeFirstSlot()
updateProgress()
selector.setSlot(1, nil)
updateTask("Defrag done", colors.lime, 100)
