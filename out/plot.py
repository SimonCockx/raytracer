import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


def read_data(file, header=0, delimiter=","):
    return pd.read_csv(file, delimiter=delimiter, header=header-1, decimal=".").values.transpose()


def init():
    data = read_data("umbraRads.csv")
    print(data)
    rays = data[0]
    radiances = data[1:]
    stds = np.std(radiances, axis=0)
    print(stds)


    fig = plt.figure()
    plt.plot(rays, stds, "b.", linewidth=3)
    # for r in radiances:
    #     plt.plot(rays, r, "b.", linewidth=3, zorder=2)
    # plt.xlim(rays[0], rays[-1])
    plt.xlabel(r"Aantal schaduwstralen", size=14)
    plt.ylabel(r"$V_o$", size=20)
    #plt.legend(fontsize=16, numpoints=1, loc=4)
    plt.tick_params(axis='both', which='major', labelsize=14)
    plt.tight_layout()
    plt.show()


init()
