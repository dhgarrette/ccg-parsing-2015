[Dan Garrette]: http://cs.utexas.edu/~dhg
[Chris Dyer]: http://www.cs.cmu.edu/~cdyer/
[Jason Baldridge]: http://www.jasonbaldridge.com
[Noah A. Smith]: http://www.cs.cmu.edu/~nasmith/

# CCG Parsing: 2015

This is the software used for the following publication:

> [Weakly-Supervised Grammar-Informed Bayesian CCG Parser Learning](http://www.cs.utexas.edu/users/ml/papers/garrette.aaai15.pdf)    
> [Dan Garrette], [Chris Dyer], [Jason Baldridge], and [Noah A. Smith]  
> In Proceedings of AAAI 2015  

> [A Supertag-Context Model for Weakly-Supervised CCG Parser Learning](http://www.cs.utexas.edu/users/ml/papers/garrette.conll15.pdf)    
> [Dan Garrette], [Chris Dyer], [Jason Baldridge], and [Noah A. Smith]  
> In Proceedings of CoNLL 2015  


## Getting the code

    $ get clone git@github.com:dhgarrette/2015-ccg-parsing.git
    $ cd 2015-ccg-parsing

## Data setup

Put the English, Chinese, and Italian data into the following directories:

     data/ccgbank
     data/ccgbank-chinese
     data/ccgbank-italian
     
The files should be arranged as follows:

	$ ls data/ccgbank/AUTO
	00	02	04	06	08	10	12	14	16	18	20	22	24
	01	03	05	07	09	11	13	15	17	19	21	23
	$ ls data/ccgbank-chinese/AUTO
	00	02	04	06	08	10	20	22	24	26	28	30
	01	03	05	07	09	11	21	23	25	27	29	31
	$ ls data/ccgbank-italian/pro
	civil_law.pro.txt	jrc_acquis.pro.txt	newspaper.pro.txt    

## Running the code

First, compile the code and generate the run script:

    $ ./compile
    
Then run:

    $ target/start dhg.ccg.run.Parse2015Run [options]

### Options

* `--model`: The model to use.  Options: (no default) 
  * `pcfg`: for the pcfg model (see AAAI-2015 paper)
  * `scg`: for the supertag-context model (see CoNLL-2015 paper) 
* `--learning`: The learning algorithm to use to train the model.  Options: {`mcmc`}.  Default: `mcmc`.
* `--additional-rules`: Additional CCG rules to be allowed by the parser (comma-separated).  Example: `FC,BX,FC2,BX2`.  Default `x` (meaning no additional rules).
* `--lang`: The language of the CCGBank to use.  Options: {`en`, `ch`, `it`}.  Default: `en`.
* `--max-sent-len`: The maximum sentence length allowed (filter all sentence longer than this).  Options: an integer or `all` for no limit.  Default: `all`.
* `--td-tok`: The maximum number of tokens to be read when building the tag dictionary.  Options: an integer (following an integer with `k` will expand to `000`; e.g. `10k` becomes `10000`) or `all` for no limit.  Default: `all`.
* `--train-sent`: The maximum number of sentences to be used for training.  Options: an integer or `all` for no limit.  Default: `all`.
* `--test-sent`: The maximum number of sentences to be used for testing.  Options: an integer or `all` for no limit.  Default: `all`.
* `--sampling-iterations`: The number of MCMC sampling iterations to run.  Default: `500`.
* `--burnin-iterations`: The number of MCMC burn-in iterations to run.  Default: `0`.
* `--alpha-root`: See paper for details.  Default: `1.0`.
* `--alpha-biny`: See paper for details.  Default: `100.0`.
* `--alpha-unry`: See paper for details.  Default: `100.0`.
* `--alpha-term`: See paper for details.  Default: `10000.0`.
* `--alpha-prod`: See paper for details.  Default: `100.0`.
* `--alpha-cntx`: See paper for details.  Only relevant for `--model scg`.  Default: `1000.0`.
* `--root-init`: Root parameter initializer. Options: 
  * `uniform`
  * `catprior`: use the grammar-defined category prior.
  * `tdecatprior`: use the grammar-defined category prior, with atomic category probabilities estimated using the tag dictionary and raw data.  DEFAULT.
* `--nt-prod-init`: Nonterminal production parameter initializer (for both binary and unary).  Options: 
  * `uniform`. 
  * `catprior`: use the grammar-defined category prior. 
  * `tdecatprior`: use the grammar-defined category prior, with atomic category probabilities estimated using the tag dictionary and raw data.  DEFAULT.
* `--term-prod-init`: Terminal production parameter initializer (for both binary and unary).  Options: 
  * `uniform`.
  * `tdentry`: Use the tag dictionary and raw data to estimate terminal (word) probabilities for each supertag.  DEFAULT.
* `--tr-init`: Context production parameter initializer (for both left and right contexts).  Only relevant for `--model scg`.  Options:
  * `uniform`, 
  * `tdentry` (use the tag dictionary and raw data to estimate transition probabilities),
  * `combine-uniform` (use CCG supertag combinability mixed with `uniform`),
  * `combine-tdentry` (use CCG supertag combinability mixed with `tdentry`).  DEFAULT.
* `--pterm`: See paper for details. Default: `0.7`.
* `--pmod`: See paper for details. Default: `0.1`.
* `--pfwd`: See paper for details. Default: `0.5`.
* `--comb-tr-mass`: Amount of probability mass devoted to "combinable" contexts (called `sigma` (σ) in the CoNLL-2015 paper).  Only relevant for `--model scg`.  Default: `0.85`.
* `--td-cutoff`: Exclude tag dictionary entries that occur with less than this proportion in the TD-training corpus.  Default: `0.0`.
* `--max-accept-tries`:  Number of samples drawn for each sentence in each iteration.  Only relevant for `--model scg`.  Default: `1`.
* `--output-file`: A file where the parsed trees of the test sentences should be written.  Default: do not write out trees.
* `--train-termdel`: Allow terminal deletion from a training sentence when a parse is not found.  Choices {`false`,`true`}.  Default: `false`.
* `--test-termdel`: Allow terminal deletion from a test sentence when a parse is not found.  Choices {`false`,`true`}.  Default: `false`.
* `--max-train-tok`: The maximum number of tokens to be read for the training data.  Options: an integer (following an integer with `k` will expand to `000`; e.g. `10k` becomes `10000`) or `all` for no limit.  Default: `all`.
* `--mcmc-output-count-file`: File where additional data should be written.  Default: do not write out this information.

