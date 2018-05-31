# dynwrap

This R package contains the code for a common model of single-cell trajectories.
Furthermore, it can:
* Classify the given topology into a trajectory type
* Contain extra data of single-cell datasets, such as expression, prior information, counts, ...
* Contain extra data specific for the trajectory inference methods, such as a clustering, dimensionality reduction, ...

The advantage of using a common model is that it allows
* Comparison between a prediction and a gold standard, eg. from [dyneval](https://www.github.com/dynverse/dyneval)
* Comparing two predictions
* Using common plotting findings, eg. from [dynplot](https://www.github.com/dynverse/dynplot)
* Extracting relevant features/genes, eg. using [dynfeature](https://www.github.com/dynverse/dynfeature)

Better documentation will be provided very soon.
