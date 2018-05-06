#!/bin/bash

source activate tensorflow_p36

export OPENAI_LOGDIR=${OPENAI_LOGDIR:-"${HOME}/openai-logs"}
export OPENAI_LOG_FORMAT=${OPENAI_LOG_FORMAT:-"tensorboard,csv"}

echo "== Start of experiments =="

time python -m baselines.deepq.experiments.run_atari --log-dir="${OPENAI_LOGDIR}/deepq/atari/0" --log-formats="${OPENAI_LOG_FORMAT}"
time python -m baselines.a2c.run_atari --log-dir="${OPENAI_LOGDIR}/a2c/atari/0" --log-formats="${OPENAI_LOG_FORMAT}"
time python -m baselines.deepq.experiments.run_atari --log-dir="${OPENAI_LOGDIR}/deepq/atari/1" --log-formats="${OPENAI_LOG_FORMAT}" --dueling=0
time python -m baselines.a2c.run_atari --log-dir="${OPENAI_LOGDIR}/a2c/atari/1" --log-formats="${OPENAI_LOG_FORMAT}" --lrschedule=linear
time python -m baselines.a2c.run_atari --log-dir="${OPENAI_LOGDIR}/a2c/atari/2" --log-formats="${OPENAI_LOG_FORMAT}" --policy=lstm
time python -m baselines.a2c.run_atari --log-dir="${OPENAI_LOGDIR}/a2c/atari/3" --log-formats="${OPENAI_LOG_FORMAT}" --lrschedule=linear --policy=lstm
time python -m baselines.a2c.run_atari --log-dir="${OPENAI_LOGDIR}/a2c/atari/4" --log-formats="${OPENAI_LOG_FORMAT}" --policy=lnlstm
time python -m baselines.a2c.run_atari --log-dir="${OPENAI_LOGDIR}/a2c/atari/5" --log-formats="${OPENAI_LOG_FORMAT}" --lrschedule=linear --policy=lnlstm

echo "== End of experiments =="
