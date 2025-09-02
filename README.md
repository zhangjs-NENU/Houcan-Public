# <p align="center">基于多阶段机器学习与 Boruta 特征筛选的 CESD-8 量表简化与测评系统</p>

## 数据说明

本项目使用的所有数据均来自中国家庭追踪调查（China Family Panel Studies，CFPS）项目，其由北京大学和国家自然科学基金委员会资助，并由北京大学中国社会科学调查中心（ISSS）实施。本项目所用数据来自2018、2020、2022年三轮调查。

*CFPS数据属于受限数据，CFPS原始数据或其衍生数据集不可以在第三方平台上公开发布。需提交申请方可获取。本人无权在此处发布原始文件及过程数据集，如需数据文件请访问[官方网站](https://www.isss.pku.edu.cn/cfps/)

在成功申请CFPS数据后，请下载2018-2022年“个人库”数据压缩后放入“RAWDATA"文件夹

<br>

## 代码说明

[R](https://www.r-project.org/) 4.5.0版本为本项目所使用的主要工具。

本项目的package管理基于renv，请先安装renv并从renv.lock恢复package及其依赖。

在成功构建环境后请运行[R/Train_Stage_Main.R](R/Train_Stage_Main.R)，以进行模型的训练和性能测试。

本项目还设计了对独立测试集的检验，如需进行，请运行[R/Predict-Database.R](R/Predict-Database.R)

<br>

## shiny应用

本项目已将构建的评价系统制作成shiny应用，目前其在线版本已发布到[shinyapp](http://zhangjs.shinyapps.io/shiny-app)

如需在本地运行，请先将训练好的模型（全部代码运行后将会出现在目录：“data/learners”）复制到“shiny-app”文件夹中再运行。

<br>
<br>
<br>
<br>

_本仓库仅供“2025 年首届‘厚粲杯’全国大学生心理与认知智能测评挑战赛”组委会进行比赛评审及交流使用_

[![CC BY-NC-ND 4.0][cc-by-nc-nd-shield]][cc-by-nc-nd]

本作品采用[知识共享署名-非商业性使用-禁止演绎 4.0 国际许可协议][cc-by-nc-nd]进行许可。

[![CC BY-NC-ND 4.0][cc-by-nc-nd-image]][cc-by-nc-nd]

[cc-by-nc-nd]: http://creativecommons.org/licenses/by-nc-nd/4.0/
[cc-by-nc-nd-image]: https://licensebuttons.net/l/by-nc-nd/4.0/88x31.png
[cc-by-nc-nd-shield]: https://img.shields.io/badge/License-CC%20BY--NC--ND%204.0-lightgrey.svg
