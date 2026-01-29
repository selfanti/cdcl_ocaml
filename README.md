尝试使用ocaml实现一个基于CDCL算法的SAT求解器   
编译命令：ocamlfind ocamlopt -linkpkg -package str sat_solver.ml -o sat_solver   
运行命令：bash run.sh <input_file_directory>
针对SAT求解器项目
CDCL SAT求解器涉及到大量递归、回溯和子句学习，性能测试时建议：

先用Landmarks进行剖析：找到最耗时的函数（如单元传播propagate、冲突分析analyze_conflict）。

再用Bechamel进行基准测试：对关键函数和不同启发式策略进行精确的耗时对比。

最后用runtime_events_tools监控GC：在求解大型CNF文件时，观察GC开销是否成为瓶颈。

例如，你可以创建一个专门的测试模块，用Bechamel包装求解器的核心函数，并使用不同规模的CNF文件作为输入进行测试。