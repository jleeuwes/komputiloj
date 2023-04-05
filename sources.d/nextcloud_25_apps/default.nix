# GENERATED
{
  value = pkgs:
  let fetchNextcloudApp = pkgs.fetchNextcloudApp;
  in {
    calendar = import ./apps/calendar { inherit fetchNextcloudApp; };
    files_linkeditor = import ./apps/files_linkeditor { inherit fetchNextcloudApp; };
  };
}
