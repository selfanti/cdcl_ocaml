#!/bin/bash

# 假设你的程序叫 my_program
PROGRAM="./sat_solver"
INPUT_DIR=$1


# 遍历目录下所有文件（不包括子目录）
for file in "$INPUT_DIR"/*; do
    if [ -f "$file" ]; then  # 确保是普通文件，不是目录
        echo "正在处理: $file"
        $PROGRAM "$file"
    fi
done
echo "输入目录为: $INPUT_DIR"