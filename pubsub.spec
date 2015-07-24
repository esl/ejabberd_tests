{suites, "tests", pubsub_SUITE}.
{config, ["test_ejabberd_pubsub.config"]}.
{logdir, "ct_report"}.
{ct_hooks, [{ct_tty_hook, [print_group, print_case]}]}.
