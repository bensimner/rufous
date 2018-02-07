import json
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

with open('data2.json') as f:
    data = json.load(f)

print(data)
plt.plot(*zip(*data), label="(random profile)")
xmax = max([x[0] for x in data])


plt.ylabel('output diff time')
plt.xlabel('size of DUG (#nodes)')
plt.title('correlation between generated DUG size and generation time') 
plt.legend()
plt.show()
