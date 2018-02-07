import json
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

with open('data1.json') as f:
    data = json.load(f)

def to_label(d):
    ws = list(sorted(d['weights']))
    ps = list(sorted(d['persistents']))
    
    weights = ', '.join('{:.1f}'.format(d['weights'][k]) for k in ws)
    persistents = ', '.join('{:.1f}'.format(d['persistents'][k]) for k in ps)
    mortality = '{:.1f}'.format(d['mortality'])
    return '([{weights}], [{persistents}], {mortality})'.format(**locals())

handles = []
fig, ax = plt.subplots(figsize=(8, 5))
for d in data:
    h, = ax.plot([x[0] for x in d[1]], [x[1] for x in d[1]], label=to_label(d[0]))
    handles.append(h)

xmax = max([x[0] for x in d[1] for d in data])

csv = ', '.join(str(x) for x in sorted(d[0]['weights']))
h = mpatches.Patch(color='white', label='key: (weights : [{}], persistents, mortality)'.format(csv))
handles.insert(0, h)


plt.ylabel('output profile error')
plt.xlabel('size of DUG (#nodes)')
plt.title('correlation between input profile and output error') 
plt.axis([1, xmax, 0, 0.5])
plt.legend(handles=handles)
plt.show()
