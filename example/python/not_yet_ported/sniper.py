"""Sniper positions itself in the center of the board and it starts scanning circularly and firing to targets."""

import json
from random import randint
from rabbit import goto, urlopen

__author__ = 'roberto'


def main():
    data = json.loads(urlopen('robot/', {'name': 'SNIPER'}, 'POST').read())
    token = data['token']
    goto(token, 500, 500)
    data = json.loads(urlopen('robot/' + token).read())
    teta = 0
    resolution = 10
    while not data['robot']['dead']:
        data = json.loads(urlopen('robot/' + token + '/scan', {'degree': teta, 'resolution': resolution}, 'PUT').read())
        distance = data['distance']
        if distance > 40:  # maximum damage radius
            data = json.loads(urlopen('robot/' + token).read())
            while data['robot']['reloading']:
                data = json.loads(urlopen('robot/' + token).read())
            json.loads(
                urlopen('robot/' + token + '/cannon', {'degree': teta, 'distance': distance}, 'PUT').read())
        else:
            teta += resolution * 2
        data = json.loads(urlopen('robot/' + token).read())
    urlopen('robot/'+token, method='DELETE')

if __name__ == '__main__':
    main()
