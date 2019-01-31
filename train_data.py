import pandas as pd

from xgboost import XGBRegressor
from xgboost import plot_importance
import xgboost

model = XGBRegressor(
    max_depth=18,
    n_estimators=55,
    min_child_weight=250,
    colsample_bytree=0.8,
    subsample=0.8,
    eta=0.3,
    seed=32,
)

dtype_dict = {
    "maker_id": "int32",
    "model_id": "int32",
    "mileage": "float64",
    "manufacture_year": "float64",
    "engine_displacement": "float64",
    "engine_power": "float64",
    "transmission_id": "int32",
    "door_count": "float64",
    "seat_count": "float64",
    "fuel_type_id": "int32",
    "price_eur": "float64",
    "date_created_timestamp": "float64",
    "date_last_seen_timestamp": "float64",
    "sale_duration": "float64",
}
train_data = pd.read_csv("data/train_data.csv", encoding="Latin-1", dtype=dtype_dict)
validation_data = pd.read_csv(
    "data/test_data.csv", encoding="Latin-1", dtype=dtype_dict
)
import ipdb

ipdb.set_trace()

X_valid = validation_data.drop(["price_eur"], axis=1)
Y_valid = validation_data["price_eur"]

X_train = train_data.drop(["price_eur"], axis=1)
Y_train = train_data["price_eur"]


model.fit(
    X_train,
    Y_train,
    eval_metric="rmse",
    eval_set=[(X_train, Y_train), (X_valid, Y_valid)],
    verbose=True,
)

import ipdb

ipdb.set_trace()
