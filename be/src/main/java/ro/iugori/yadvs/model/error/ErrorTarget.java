package ro.iugori.yadvs.model.error;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import ro.iugori.yadvs.model.domain.TargetType;

import java.io.Serializable;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class ErrorTarget implements Serializable {

    private TargetType type;
    private String name;

}