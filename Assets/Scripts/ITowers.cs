using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public interface ITowers
{
    void Shoot(GameObject bullet);
    void TargetUpdate();
    void Rotation();
}
