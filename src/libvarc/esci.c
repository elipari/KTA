extern void
exit_(int *exitstatus)
{
  f_exit();
  exit(exitstatus ? (int)*exitstatus : 0);
}
