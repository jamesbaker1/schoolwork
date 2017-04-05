# NO IMPORTS!

# Pack a tent with different sleeping bag shapes leaving no empty squares
#
# INPUTS:
#   tent_size = (rows,cols) for tent grid
#   missing_squares = set of (r,c) tuples giving location of rocks
#   bag_list = list of sets, each decribing a sleeping bag shape
#      Each set contains (r,c) tuples enumerating contiguous grid
#      squares occupied by bag, coords are relative to the upper-
#      left corner of bag.  You can assume every bag occupies
#      at least the grid (0,0).
#
# Example bag_list entries:
#      vertical 3x1 bag: { (0,0), (1,0), (2,0) }
#      horizontal 1x3 bag: { (0,0), (0,1), (0,2) }
#      square bag: { (0,0), (0,1), (1,0), (1,1) }
#      L-shaped bag: { (0,0), (1,0), (1,1) }
#      C-shaped bag: { (0,0), (0,1), (1,0), (2,0), (2,1) }
#      reverse-C-shaped bag: { (0,0), (0,1), (1,1), (2,0), (2,1) }
#
# OUTPUT:
#   None if no packing can be found
#   a list giving the placement and type for each placed bag
#   expressed as a dictionary with keys
#     "anchor": (r,c) for upper-left corner of bag
#     "shape": index of bag on bag list
def is_board_valid(tent_size, missing_squares, sleeping_bags, bag_list):
	'''Check to see if each person lies entirely in the tent,
	   no person is sleeping on a rock, and 
	   no two people have a square in common'''
	squares_with_people = []
	#Check to see if each person's anchor square is in the tent
	for i in range(len(sleeping_bags)):
		# if sleeping_bags[i]["anchor"][0] < 0 or sleeping_bags[i]["anchor"][0] >= tent_size[0]:
		# 	return False
		# if sleeping_bags[i]["anchor"][1] < 0 or sleeping_bags[i]["anchor"][1] >= tent_size[1]:
		# 	return False
		sleeping_bag_coords = []
		for j in bag_list[sleeping_bags[i]["shape"]]:
			sleeping_bag_coords.append(((sleeping_bags[i]["anchor"][0] + j[0]), (sleeping_bags[i]["anchor"][1] + j[1])))
			squares_with_people.append(((sleeping_bags[i]["anchor"][0] + j[0]), (sleeping_bags[i]["anchor"][1] + j[1])))
		#Check to see if each person lies entirely in the tent
		for coord in sleeping_bag_coords:
			if coord[0] < 0 or coord[0] >= tent_size[0]:
				return False
			if coord[1] < 0 or coord[1] >= tent_size[1]:
				return False
			if coord in missing_squares:
				return False
	#Check to see that no one shares the same square
	if len(squares_with_people) != len(set(squares_with_people)):
		return False
	return True

def finished(tent_size, missing_squares, sleeping_bags, bag_list):
	''' Check to see if the game is finished, if not, return 
	    a square where we can place the next sleeping bag'''
	squares_with_people = set()
	#Render the tent as a 2 dimensional board filled with 0s as an empty space and 
	#1s as a spaces that contain a person or a rock
	for i in range(len(sleeping_bags)):
		for j in bag_list[sleeping_bags[i]["shape"]]:
			squares_with_people.add(((sleeping_bags[i]["anchor"][0] + j[0]), (sleeping_bags[i]["anchor"][1] + j[1])))
	tent = []
	for i in range(tent_size[0]):
		row = []
		for j in range(tent_size[1]):
			if (i,j) in missing_squares:
				row.append(1)
			elif (i,j) in squares_with_people:
				row.append(1)
			else:
				row.append(0)
		tent.append(row)
	#Go through board and check to see if there are any spaces open,
	#if there are, return False and the space coordinate
	for row in range(len(tent)):
		for col in range(len(tent[0])):
			if tent[row][col] == 0:
				return (False, (row,col))
	return (True, None)

def pack(tent_size, missing_squares, bag_list):
	def recursion_function(tent_size, missing_squares, bag_list, sleeping_bags):
		'''Created a new recursive function that has sleeping bags as an argument'''
		#Check to see if we finished, if not, obtain a square we can place a sleeping bag at
		x , p = finished(tent_size, missing_squares, sleeping_bags, bag_list)
		if is_board_valid(tent_size, missing_squares, sleeping_bags, bag_list) and x:
			return sleeping_bags
		sleeping_bags1 = sleeping_bags[:]
		#Go through each sleeping bag type, and call the function with the new sleeping bag list
		for i in range(len(bag_list)):
			sleeping_bags1.append({"anchor": p, "shape": i})
			if is_board_valid(tent_size, missing_squares, sleeping_bags1, bag_list):
				y = recursion_function(tent_size, missing_squares, bag_list, sleeping_bags1)
				if y:
					return y
			#If the sleeping bag does not work, try again
			del sleeping_bags1[-1]
		return None
	return recursion_function(tent_size, missing_squares, bag_list, [])




