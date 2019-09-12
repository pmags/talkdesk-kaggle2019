#%%

# import libraries
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

#%% [markdown]
# Challenge and variables description

## Problem

The number of agents available is crucial to the Contact Center success operation, where a low number of agents will lead to long waiting queues for the contacts and a high number will increase the operation cost by having agents doing nothing.

**The dataset corresponds to Contact Center metrics aggregated in 15 minutes intervals for a real Talkdesk client between 2017-01-01 and 2018-11-30 (23 months), and the competition goal is to forecast the number of agents for the days between 2018-12-01 and 2018-12-31 (1 month).**

## Features

The following information is provided with the data:

- *Interval* - the date and time corresponding to the contact center activity. This is given on a 15 min interval. 

### Events:

During the initial presentation we learnt that talkdesk runs an "event" based operation. **A call is an event, and event triggers actions. The duration of each event is a restriction to the number of workers.** 

For each event information about quantity and duration is given:

- *Total Calls* - the number of calls. This represents the total amount of calls received during the time period. For example `r paste("between",data$interval[1],"and",data$interval[1],"this specific costumer received ", data$total_calls[1], "calls")`. 

- *Total Calls Duration* - the duration of a calls in seconds. This means `r paste("that on the", data$interval[1],"a total of",data$total_calls[1],"where received and took the total of",data$total_calls_duration[1], "seconds or in hours", data$total_calls_duration[1]/3600)`

- *Abandoned Calls* / *missing_calls*  - the number of calls that reach Talkdesk platform but hung up before it was answered by an agent. No further information regarding the reason why the call was missed is given. This may or may not impact the solution. For example, if miss calls are due to all operators being occupied, then, in order to maximize the service it may pay off the hire an extra worker.

> This feature raises the question of how long is an event. It makes sense that we take into account that one event requires a 1. setup time, an 2. execution time, a 3. after event task and probably a 4. cool down between events. 

### Workers:

Being a service, "events" take time as raw material to be completed. This time is provided by workers which can supply a limited amount of it per day. The data provides us information regarding quantity for each time interval (headcount and available time) and how it is distributed by "tasks". 

From the information provided we can deduce 


### Metrics:
