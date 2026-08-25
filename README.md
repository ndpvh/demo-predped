# Demo of `predped`

This repository serves as a demo of the functionalities of the [`predped`](https://github.com/ndpvh/predped) package.



## Background

Pedestrian models are popular tools to investigate how people navigate the complex world that we live in. Yet, most current models assume that pedestrians are all one and the same, making them solely applicable to study movement behavior in high-density situations like festivals or sporting events, but less so in low-density situations like supermarkets and train stations.

To capture movement behavior in both low- and high-density situations, Andrew Heathcote and Dora Matzke recently proposed the M4MA, proposing that pedestrian behavior is determined on three levels:

- Strategic level: Consists of what the agent want to achieve and how they will navigate the space;
- Tactical level: Consists of reacting to the environment and making tactical decisions regarding navigation in case of blockages;
- Operational level: Consists of the moment-to-moment step decisions the agent takes, encompassing changes in direction and/or speed at the lowest level.
Critically, M4MAs pedestrians have a particular “personality” reflected in unique parameters that guide the movement on the operational level. These individual differences are implemented in two ways:

- Qualitative differences: Each pedestrian belongs to a particular class of people, defining a particular pedestrian profile that one may encounter within the setting of interest;
- Quantitative differences: Within each class, parameter values are subject to random variation, assigning each pedestrian with a unique combination of parameters.



## Content

This repository accompanies a tutorial on [`predped`](https://github.com/ndpvh/predped) that can be found here XXX, containing scripts that showcase its functionality. The tutorial code is subdivided into three scripts that can be found under the folder `scripts`, each focusing on a particular part of [`predped`](https://github.com/ndpvh/predped):

- `building_blocks.R`: This script contains a basic introduction to the logic of [`predped`](https://github.com/ndpvh/predped) and the `S4` class that it builds on;
- `simulation.R`: This file contains both basic and advanced simulations, showing how to generate data with the M4MA;
- `estimation.R`: This file shows how one can estimate the M4MA on empirical and simulated data.

For more detailed information on [`predped`](https://github.com/ndpvh/predped), we refer the interested reader to the accompanying tutorial (XXX) or to the documentation site of the package (https://ndpvh.github.io/predped).



## Getting started

To use this code, one should first install the latest version of [`predped`](https://github.com/ndpvh/predped) via the call:

```
remotes::install_github("ndpvh/predped")
```

One can then make use of [`predped`](https://github.com/ndpvh/predped) through a call to `library()`:

```
library(predped)
```



## License

This project is shared under the GNU GPL-3.0 license. See the [LICENSE](https://github.com/ndpvh/demo-predped/blob/main/LICENSE) for more information on what this means.



## Contribute

If you wish to contribute to this demo or feel that some functionalities are not sufficiently included in this repository, feel free to open an [Issue](https://github.com/ndpvh/demo-predped/issues).