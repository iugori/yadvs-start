package ro.iugori.yadvs.util.mapping;

import ro.iugori.yadvs.dto.Poll;
import ro.iugori.yadvs.model.entity.PollEntity;

public class PollMapper {

    public static void put(Poll dto, PollEntity entity) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        entity.setStatus(dto.getStatus());
    }

    public static void patch(Poll dto, PollEntity entity) {
        if (dto.getId() != null) {
            entity.setId(dto.getId());
        }
        if (dto.getName() != null) {
            entity.setName(dto.getName());
        }
        if (dto.getDescription() != null) {
            entity.setDescription(dto.getDescription());
        }
        if (dto.getStatus() != null) {
            entity.setStatus(dto.getStatus());
        }
    }

    public static Poll dtoFrom(PollEntity entity) {
        var dto = new Poll();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setStatus(entity.getStatus());
        return dto;
    }


}
