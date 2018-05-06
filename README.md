# Machine Learning Nanodegree capstone - Solving Breakout with Deep Reinforcement Learning

In this capstone project I evaluate the algorithms DQN e A2C to make an AI that
learns how to play Breakout.

In this project I used:
- OpenAI Gym.
- OpenAI Baselines adapted for the project, in this package. I left this working
copy in the state I ran the experiments.
- TensorFlow
- cloudpickle
- numpy
- pandas
- scipy
- joblib

There is an AWS EC2 Instance ready for this and **public**: ami-1c79d264. This
AMI have all needed packages to run the experiment and a shell script to
facilitate the work. It is described in the report how to use it.

I could not use IPython notebooks because *baselines* package was not practical
to work with Jupyter notebooks and I had to extract the results from
TensorBoard and CSVs.
