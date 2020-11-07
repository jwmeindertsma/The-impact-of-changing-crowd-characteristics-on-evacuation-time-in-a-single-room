# Importing packages
from additional_functions import *
import numpy as np
import math
import numpy.random as random
# Setting seed
random.seed(123)
# Creating a dataset
nr_agents = 75
nr_experiments = 10
# Walls list to check
room_height = 400 # height of the room
room_width = 400 # width of the room
room_left = 100 # left pixels coordinate
room_top = 100 # top pixels coordeinate

# Door 1
door_ytop = 282
door_ybottom = 318

side = math.sqrt(36**2/(3/4))


walls = [[room_left, room_top, room_left + room_width, room_top], 
    [room_left, room_top, room_left, door_ytop], 
    [room_left, room_top+room_height, room_left, door_ybottom],
    [428,300-side/2,428,300+side/2], [428,300-side/2,464,300], [428,300+side/2,464,300],  # additional walls  # additional walls
    [room_left, room_top+room_height, room_left + room_width, room_top+ room_height],
    [room_left + room_width, room_top, room_left + room_width, door_ytop],
    [room_left+room_width, room_top + room_height, room_left + room_width, door_ybottom]]

# List to save positions
positionmatrix = []
# For all experiments
for j in range(0,nr_experiments):
    nr_experiment = j + 1
    agents_found = 0 
    for i in range(0,nr_agents): # For all objects   
        # We start by finding a random position in the room 
        found = False
        countwall = 0
        while found == False:
            countwall = 0
            desiredS =  24
            mass = 80  #np.random.uniform(60,100) #80 # 100
            radius = 12/80 * mass
            placeradius = 1
            object_x = np.random.uniform(100,500)
            object_y = np.random.uniform(100,500)
            for wall in walls:
                r_i = placeradius
                d_iw,e_iw = distance_agent_to_wall(np.array([object_x, object_y]),wall)
                if d_iw < r_i:
                    countwall += 1
            
            if len([positionmatrix[i] for i in range(j*nr_agents, j*nr_agents + agents_found)]) > 0:
                countagents = 0
                for position in [positionmatrix[i] for i in range(j*nr_agents, j*nr_agents + agents_found)]:
                    dist = math.sqrt((position[0]-object_x)**2 + (position[1]-object_y)**2)
                    if dist > position[6] + placeradius: 
                        countagents += 1
                if countagents == i and countwall == 0:
                    found = True
                    agents_found += 1 
            elif countwall == 0:
                found = True
                agents_found += 1 
    
        positionmatrix.append([object_x, object_y, radius, mass, desiredS, nr_experiment, placeradius])