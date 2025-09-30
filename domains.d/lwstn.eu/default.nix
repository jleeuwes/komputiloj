{
    name = "lwstn.eu";
    provider = "transip";
    records = builtins.readFile ./records;
}
