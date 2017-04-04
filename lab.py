# No imports allowed


def help_maze(dimensions):
    ''' 
    Create a board with the given dimensions, filled with Xs.

    :param dimensions: list, given board dimension
    :return: 2d array with given dimensions, filled with Xs
    '''
    board = []
    for i in range(dimensions[0]):
        row = []
        for j in range(dimensions[1]):
            row.append("X")
        board.append(row)
    return board


def find_all_maze_path(graph, start, end, path=[]):
    ''' 
    Recursive function to return all the given paths
    from a certain start point to end point.

    :param graph: a dictionary with keys as coordinates and values
    as coordinates that can be reach from the key
    :param start: tuple, start coordinate
    :param end: tuple, end coordinate

    :return: list of lists of paths 
    '''
    path = path + [start]
    if start == end:
        return [path]
    paths = []
    for node in graph.get(start, []):
        if node not in path:
            paths.extend(find_all_maze_path(graph, node, end, path))
    return paths

def create_recursion_dict(m):
    """ 
    Create a dictionary with keys as coordinates and values
    as coordinates that can be reach from the key

    :param m: a dictionary represnting a maze with keys "dimensions and "maze"
    :returns: a dictionary that can be parsed thorugh a recursive function
    """
    new_dict = {}
    for r in range(len(m["maze"])):
        for c in range(len(m["maze"][0])):
            if m["maze"][r][c] != 1:
                new_dict[(r, c)] = []
                if r + 1 < m["dimensions"][0] and m["maze"][r + 1][c] != 1:
                    new_dict[(r, c)].append((r + 1, c))
                if c + 1 < m["dimensions"][1] and m["maze"][r][c + 1] != 1:
                    new_dict[(r, c)].append((r, c + 1))
    return new_dict

def solved_maze_helper(m):
    '''Initiate a new dictionary fill with dimensions of given 
       dictionary and board will with Xs
       
       :param m: a dictionary represnting a maze with keys "dimensions and "maze"

       :return: a dictionary with two keys "dimensions" and "maze". "maze" value
       is a 2d array filled with Xs
    '''
    solved_maze = dict()
    solved_maze["dimensions"] = m["dimensions"]
    solved_maze["maze"] = help_maze(m["dimensions"])
    return solved_maze




def solve_maze(m, start, goal):
    """
    Find the paths from start to goal in a maze m that maximize the number of coins collected.

    :param m: a dictionary representing a maze with keys "dimensions" and "maze".
        > "dimensions" points to an array [nrows, ncols] corresponding to the dimensions of maze
        > "maze" points to an array of arrays with structure corresponding to the rows of the maze where
            - 0 represents empty space
            - 1 represents a wall
            - "c" represents a coin
            - "b" represents a bomb
    :param start: array in form [r, c]; starting point in the maze
    :param goal: array in form [r, c]; destination point in the maze
    :return: a dictionary result with the same structure as m
        where result["maze"][r][c], for 0<= r < nrows, 0<= c < ncols is
             "X" if no valid paths exist along path start --> (r, c) --> goal
             otherwise max number of coins collected on any valid path from start --> (r, c) --> goal
    """
    solved_maze, new_dict = solved_maze_helper(m), create_recursion_dict(m)
    maze_paths = find_all_maze_path(new_dict, tuple(start), tuple(goal))
    start_row = start[0]
    start_col = start[1]
    if maze_paths == []:
        return solved_maze
    if m["maze"][start_row][start_col] == "c":
        solved_maze["maze"][start_row][start_col] = 1
    if m["maze"][start_row][start_col] == "b":
        solved_maze["maze"][start_row][start_col] = 0
    for paths in maze_paths:
        count = 0
        for coord in paths:
            row = coord[0]
            col = coord[1]
            if m["maze"][row][col] == "c":
                count += 1
                if type(solved_maze["maze"][row][col]) == str or count > solved_maze["maze"][row][col]:
                    solved_maze["maze"][row][col] = count
            if m["maze"][row][col] == "b":
                count = 0
                if type(solved_maze["maze"][row][col]) == str or count > solved_maze["maze"][row][col]:
                    solved_maze["maze"][row][col] = count
            if m["maze"][row][col] == 0:
                if type(solved_maze["maze"][row][col]) == str or count > solved_maze["maze"][row][col]:
                    solved_maze["maze"][row][col] = count
    return solved_maze


# Bonus Section (not required portion of the lab)
def find_max_path(out_m, start, goal):
    """
    Return a path that results in the highest possible number of coins collected from start to goal.
    In case of ties, return any such path.

    :param out_m: output from solve_maze function, a maze dictionary
        where out_m["maze"][r][c], for 0<= r < nrows, 0<= c < ncols is
             "X" if no valid paths exist along path start --> (r, c) --> goal
             otherwise max number of coins collected on any valid path from start --> (r, c) --> goal
    :param start: array in form [r, c]; starting point in the maze
    :param goal: array in form [r, c]; destination point in the maze
    :return: list of tuples of coordinates from start to goal that results in highest coins collected if path exists
             None if no path exists from start to goal
    """
    new_dict, maze_path = create_recursion_dict(m), find_all_maze_path(new_dict, tuple(start), tuple(goal))
    if maze_path == None:
        return None
    best_path = []
    best_count = 0
    for path in maze_path:
        count = out_m["maze"][path[0][0]][path[0][1]]
        for coord in range(1, len(path)):
            row = path[coord][0]
            col = path[coord][1]
            if count + 1 == out_m["maze"][row][col] or count == out_m["maze"][row][col] or out_m["maze"][row][col] == 0:
                count = out_m["maze"][row][col]
                if count > best_count and len(path) > coord + 1 and out_m["maze"][path[coord + 1][0]][path[coord + 1][1]] != 0:
                    best_count = count
                    best_path = path

    return best_path



def pretty_print(m):
    """
    Prints a visual representation of a maze (useful for debugging)
    :param m: dictionary, maze
    """
    nrows, ncols = m["dimensions"]
    print("\n > Dimensions: ", m["dimensions"], "\n")
    print(" " + "-" * 3 * ncols)
    for r in range(nrows):
        line = ""
        for c in range(ncols):
            elt = m["maze"][r][c]
            if c == 0:
                line += "|"
            if type(elt) == int:
                if elt >= 10:
                    line += " " + str(elt)
                else:
                    line += " " + str(m["maze"][r][c]) + " "
            else:
                line += " " + str(m["maze"][r][c]) + " "
            if c == ncols - 1:
                line += "|"
        print(line)
    print(" " + "-" * 3 * ncols + "\n")



