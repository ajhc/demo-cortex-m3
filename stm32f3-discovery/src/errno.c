int errno;

int *__errno()
{
    return &errno;
}

