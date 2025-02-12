import numpy as np
import pandas as pd

subscriber_data = {
    "Constellation": [
        "Astra", "BlueWalker", "Cinnamon-937", "Flock", "Globalstar",
        "Guowang", "Hanwha", "Honghu-3", "HVNET", "KLEO", "Kuiper",
        "Lacuna", "Lightspeed", "Lynk", "Omni", "OneWeb", "Rassvet",
        "Semaphore-C", "SferaCon", "Starlink (Gen2)", "Swarm",
        "Xingshidai", "Xingwang", "Yinhe"
    ],
    "Subscribers": [
        6666990, 118949, 165119609, 73425, 1507660, 6359584, 979000, 4895000,
        704880, 146850, 1582064, 117480, 146850, 979000, 97900, 3469576,
        440550, 57095280, 313280, 19284342, 73425, 97900, 472857, 489500
    ]
}

emissions_data = {
    "Constellation": [
        "Astra", "BlueWalker", "Cinnamon-937", "Flock", "Globalstar",
        "Guowang", "Hanwha", "Honghu-3", "HVNET", "KLEO", "Kuiper",
        "Lacuna", "Lightspeed", "Lynk", "Omni", "OneWeb", "Rassvet",
        "Semaphore-C", "SferaCon", "Starlink (Gen2)", "Swarm",
        "Xingshidai", "Xingwang", "Yinhe"
    ],
    "Total_Emissions": [
        5706.8, 105.6, 69931.9, 3.3, 2585.7, 383.6, 156.9, 3021.9, 345.5, 90.8, 
        172.8, 95.3, 16.7, 166.4, 16.6, 380.2, 241.9, 24711.9, 171.3, 3717.6, 
        11.6, 15.3, 197.2, 103.3
    ]
}

df_subscribers = pd.DataFrame(subscriber_data)
df_emissions = pd.DataFrame(emissions_data)

df_combined = df_subscribers.merge(df_emissions, on="Constellation")

def monte_carlo_emissions_per_subscriber(subscribers, total_emissions, num_simulations=1000):
    results = []
    for _ in range(num_simulations):
        low = subscribers * 0.9
        hi