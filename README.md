尝试使用ocaml实现一个基于CDCL算法的SAT求解器
编译命令：ocamlfind ocamlopt -linkpkg -package str sat_solver.ml -o sat_solver
运行命令：bash run.sh <input_file_directory>