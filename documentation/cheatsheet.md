# Run

### T1

#### batch 

```bash
kubectl delete gameservers --all && kubectl delete pods --all \
&& cd ~/cloudsim_k8s_launcher \
&& kubectl apply -f ./cloudsim_k8s_launcher.yaml \
&& mongo roslog --eval "printjson(db.dropDatabase())" \
&& roslaunch knowrob knowrob.launch
```

#### knowrob 
```bash
roslaunch knowrob knowrob.launch
```

#### purge k8s and roslog
```bash
kubectl delete gameservers --all && kubectl delete pods --all \
&& cd ~/cloudsim_k8s_launcher \
&& kubectl apply -f ./cloudsim_k8s_launcher.yaml \
&& mongo roslog --eval "printjson(db.dropDatabase())"
```

### T2

```bash
rosrun rosprolog rosprolog_commandline.py
```

```prolog
register_ros_package('knowrob_ameva'), ue_start_srv.
```

# EspressoCup / DrawerForce / DishwasherForce

```prolog
am_semantic_map:am_get_drawer_stack_max(Num,'EC_128_LF_SL','IAIPR2CSMapytG128SL','http://knowrob.org/kb/knowrob.owl#EspressoCup','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 25, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'EC_128_SL','IAIPR2CSMapytG128SL','http://knowrob.org/kb/knowrob.owl#EspressoCup','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 15, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'EC_128_LF','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#EspressoCup','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 25, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'EC_128_3','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#EspressoCup','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 15, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'EC_128_UD_LF','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#EspressoCup','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 25, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'EC_128_UD','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#EspressoCup','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 15, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).
```

## SmallBowl

```prolog
am_semantic_map:am_get_drawer_stack_max(Num,'SB_128_LF_SL','IAIPR2CSMapytG128SL','http://knowrob.org/kb/knowrob.owl#SmallBowl','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 25, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'SB_128_SL','IAIPR2CSMapytG128SL','http://knowrob.org/kb/knowrob.owl#SmallBowl','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 15, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'SB_128_LF','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#SmallBowl','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 25, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'SB_128','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#SmallBowl','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 15, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).
```

## EspressoSaucer

```prolog
am_semantic_map:am_get_drawer_stack_max(Num,'ES_128_LF_SL','IAIPR2CSMapytG128SL','http://knowrob.org/kb/knowrob.owl#EspressoSaucer','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 25, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'ES_128_SL','IAIPR2CSMapytG128SL','http://knowrob.org/kb/knowrob.owl#EspressoSaucer','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 15, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'ES_128_LF_2','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#EspressoSaucer','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 25, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'ES_128_5','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#EspressoSaucer','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 15, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).
```

## ClassicPlate16cm

```prolog
am_semantic_map:am_get_drawer_stack_max(Num,'CP16_LF_2','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#ClassicPlate16cm','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 25, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).

am_semantic_map:am_get_drawer_stack_max(Num,'CP16_128_3','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#ClassicPlate16cm','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 15, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).
```

## ClassicPlate23cm

```prolog
am_semantic_map:am_get_drawer_stack_max(Num,'CP23_128_LF','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#ClassicPlate23cm','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 25, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).


am_semantic_map:am_get_drawer_stack_max(Num,'CP23_128','IAIPR2CSMapytG128','http://knowrob.org/kb/knowrob.owl#ClassicPlate23cm','http://knowrob.org/kb/ameva_log.owl#5hJLcyxGBEaaac6EwzeRQQ', 15, 0, 0, 45, 0, 0, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', 5, 0, 0, 1).
```

## Misc

```prolog
am_semantic_map:am_load_semantic_map('IAIPR2CSMapytE', MapInst), am_semantic_map:am_get_level_name(MapInst, LevelName).

am_semantic_map:am_load_semantic_map('IAIPR2CSMapytE', MapInst), am_semantic_map:am_get_translation(MapInst, 'http://knowrob.org/kb/ameva_log.owl#eiueD8tubk-Sp7yA-iPlHg', X, Y, Z).

am_semantic_map:am_load_semantic_map('IAIPR2CSMapytE', MapInst), am_semantic_map:am_get_translation(MapInst, 'http://knowrob.org/kb/ameva_log.owl#MXcLSBHNyUWQsTN847usYw', X, Y, Z).

am_semantic_map:am_load_semantic_map('IAIPR2CSMapytE', MapInst), am_semantic_map:am_get_translation(MapInst, 'http://knowrob.org/kb/ameva_log.owl#_0Dwd6opXUyImm9_yY0uCw', X, Y, Z).

am_semantic_map:am_get_height('http://knowrob.org/kb/knowrob.owl#IAICeilingLight', Height).
```

## Hard reboot batch

```bash
yes | sudo kubeadm reset \
&& sudo swapoff -a \
&& sudo kubeadm init --pod-network-cidr=10.244.0.0/16 \
&& mkdir -p $HOME/.kube \
&& yes | sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config \
&& sudo chown $(id -u):$(id -g) $HOME/.kube/config \
&& kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel.yml \
&& kubectl taint nodes --all node-role.kubernetes.io/master- && kubectl create namespace agones-system \
&& kubectl apply -f https://raw.githubusercontent.com/googleforgames/agones/release-1.10.0/install/yaml/install.yaml \
&& kubectl create clusterrolebinding default-view --clusterrole=view --serviceaccount=default:default \
&& kubectl create clusterrolebinding serviceaccounts-cluster-admin --clusterrole=cluster-admin --group=system:serviceaccounts \
&& cd ~/cloudsim_k8s_launcher && kubectl apply -f ./cloudsim_k8s_launcher.yaml \
&& sudo systemctl start mongod
```

## Debug

```bash
kubectl get pods
watch -n 1 nvidia-smi
```


## Docker

### setup

* `cd /path/RobCog`

* `docker login` user robcog: https://hub.docker.com/u/robcog

* if `ue4-full` is missing (github login required for ue4 source access)

```bash
ue4-docker build custom:4.23.1-pixelstreaming -repo=https://github.com/adamrehn/UnrealEngine.git -branch=4.23.1-pixelstreaming --no-engine
```

### build map

```bash
docker build -t robcog/cs_pr2dishwasher_fix128 . 
docker push robcog/cs_pr2dishwasher_fix128

docker build -t robcog/cs_pr2dishwasher_fix128_sl . 
docker push robcog/cs_pr2dishwasher_fix128_sl

```
