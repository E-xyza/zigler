const ExternalErrorType = error { ExternalError, };

pub fn void_error() !void {
    return error.ExternalError;
}