package ro.iugori.yadvs.delegate.rest;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.ConstraintViolation;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.hateoas.Link;
import ro.iugori.yadvs.model.ctx.CallContext;
import ro.iugori.yadvs.model.error.*;
import ro.iugori.yadvs.model.rest.ctx.RestContext;
import ro.iugori.yadvs.model.rest.sturctured.ErrorResponse;

import java.util.Optional;
import java.util.Set;

public class ErrorResponseBuilder {

    private static ErrorResponse of(CallContext callCtx) {
        var path = "N/A";
        if (callCtx instanceof RestContext restCtx && restCtx.getRequest() != null) {
            path = restCtx.getRequest().getRequestURI();
        }
        return new ErrorResponse(callCtx.getLogRef(), callCtx.getTimeRef(), path);
    }

    private static ErrorResponse of(CallContext callCtx, Exception e) {
        var error = new ErrorModel();
        error.setCode(ErrorCode.of(e));
        error.setMessage(e.getMessage());
        var errors = of(callCtx);
        errors.add(error);
        return errors;
    }

    public static ErrorResponse of(YadvsRestException yadvsEx) {
        var exCause = yadvsEx.getCause();

        var errors = of(yadvsEx.getCallCtx());

        if (exCause instanceof CheckException checkEx) {
            var swaggerUrl = getSwaggerUrl(((RestContext) yadvsEx.getCallCtx()).getRequest());
            for (var error : checkEx.getErrors()) {
                var errorClone = SerializationUtils.clone(error);
                errorClone.add(Link.of(swaggerUrl, "swagger"));
                errors.add(errorClone);
            }
        } else {
            var ex = exCause == null ? yadvsEx : exCause;
            var error = new ErrorModel();
            error.setCode(ErrorCode.of(ex));
            error.setMessage(ex.getMessage());
            errors.add(error);
        }

        return errors;
    }

    public static ErrorResponse of(RestContext restCtx, Exception e, TargetType targetType, String targetName) {
        var errors = of(restCtx, e);
        var error = errors.getError(0);
        if (error.getLinks().isEmpty()) {
            error.add(Link.of(getSwaggerUrl(restCtx.getRequest()), "swagger"));
        }
        error.setTarget(targetType, targetName);
        return errors;
    }

    public static <T> ErrorResponse of(RestContext restCtx, Set<ConstraintViolation<T>> validationResult) {
        var errors = of(restCtx);
        validationResult.forEach(constraint -> {
            var error = ErrorModel.of(constraint);
            error.add(Link.of(getSwaggerUrl(restCtx.getRequest()), "swagger"));
            errors.add(error);
        });
        return errors;
    }


    private static String getSwaggerUrl(HttpServletRequest request) {
        var url = new StringBuilder(String.format("%s://%s:%d",
                request.getScheme(), request.getServerName(), request.getServerPort()));
        Optional.ofNullable(request.getContextPath())
                .filter(StringUtils::isNotEmpty)
                .map(path -> url.append("/").append(path));
        url.append("/swagger-ui.html");
        return url.toString();
    }

}
